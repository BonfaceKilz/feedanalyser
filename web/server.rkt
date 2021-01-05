#lang racket

(require (planet dmac/spin)
         algorithms
         web-server/templates
         web-server/http
         web-server/http/cookie
         web-server/http/cookie-parse
         json
         redis
         threading
         "common.rkt"
         "arxiv.rkt"
         "github.rkt"
         "pubmed.rkt"
         "twitter.rkt")

(provide start-server)

(define (redis-output->json redis/output)
  (with-output-to-string
    (lambda ()
      (write-json (map (lambda (h)
                   (make-hash
                    (zip-with
                     (lambda (x y) `(,x . ,y))
                     (~>> (hash-keys h)
                          (map bytes->string/utf-8)
                          (map string->symbol))
                     (~>> (hash-values h)
                          (map
                           (lambda (x)
                             (with-handlers ([exn:fail?
                                              (lambda (exn)
                                                (bytes->string/utf-8 x))])
                               (bytes->string/latin-1 x))))))))
                 (filter-not hash-empty? redis/output))))))

(define (extract-cookie req cookie-key)
  (define cookies (request-cookies req))
  (define cookie-find
    (findf (lambda (c)
             (string=? cookie-key (client-cookie-name c)))
           cookies))
  (if cookie-find
      cookie-find
      #f))

(define (create-cookie key val
                       #:path [path "/"]
                       #:expires [expires
                                  (seconds->date
                                   (+ (* 48 3600)
                                      (current-seconds)))])
    (cookie->header (make-cookie
                     key
                     val
                     #:path path
                     #:expires expires)))

(define (track-per-user-vote req hash)
  (let* ([hash-cookie (string-replace
                       hash ":" "--")]
         [cookie (extract-cookie req hash-cookie)]
         (vote (hash-ref (~> req
                             request-post-data/raw
                             bytes->jsexpr)
                        'vote))
         [cookie-hash-value
          (number->string
           (if
            cookie
            (+ (string->number
                (client-cookie-value cookie))
               (if (string=? vote "upvote") 1 0))
            (if (string=? vote "upvote") 1 0)))])
    `(,(create-cookie hash-cookie cookie-hash-value)
      ,(string->number cookie-hash-value))))

;; When starting the server, inject, the REDIS client
(define (start-server
         client
         #:port [port 8000]
         #:log-file [log-file "feed.log"]
         #:feed-prefix [feed-prefix ""])

  (get "/"
       (lambda (req)
         (let ([articles
                (redis-output->json
                 (get-items/redis
                  client
                  #:key "pubmed-score:"
                  #:feed-prefix feed-prefix))]
               [tweets
                (redis-output->json
                 (truncate-spammy-tweets
                  (get-items/redis client
                                   #:key "tweet-score:"
                                   #:feed-prefix feed-prefix)))]
               [arxiv-articles
                (redis-output->json
                 (get-items/redis
                  client
                  #:key "arxiv-score:"
                  #:feed-prefix feed-prefix))]
               [commits
                (redis-output->json
                 (get-items/redis
                  client
                  #:key "commit-score:"
                  #:feed-prefix feed-prefix))])
           (include-template "templates/voting.html"))))

  (post "/update-cookies"
        (lambda (req)
          (let* ([json/vals (bytes->jsexpr (request-post-data/raw req))]
                 [tweet-order (hash-ref json/vals 'tweet-order)]
                 [tweet-select-by (hash-ref json/vals 'tweet-select-by)]
                 [commit-order (hash-ref json/vals 'commit-order)]
                 [commit-select-by (hash-ref json/vals 'commit-select-by)]
                 [pubmed-order (hash-ref json/vals 'pubmed-order)]
                 [pubmed-select-by (hash-ref json/vals 'pubmed-select-by)]
                 [arxiv-order (hash-ref json/vals 'arxiv-order)]
                 [arxiv-select-by (hash-ref json/vals 'arxiv-select-by)]
                 [cookies-list
                  (map (lambda (el)
                         (create-cookie (car el) (cadr el)))
                       (list `("tweet-order" ,tweet-order)
                             `("tweet-select-by" ,tweet-select-by)
                             `("commit-order" ,commit-order)
                             `("commit-select-by" ,commit-select-by)
                             `("pubmed-order" ,pubmed-order)
                             `("pubmed-select-by" ,pubmed-select-by)
                             `("arxiv-order" ,arxiv-order)
                             `("arxiv-select-by" ,arxiv-select-by)
                             ))])
            `(201 ,cookies-list "OK"))))

  (post "/vote/tweets"
        (lambda (req)
          (let* ([json/vals (bytes->jsexpr (request-post-data/raw req))]
                 [tweet-hash (hash-ref json/vals 'hash)]
                 [vote (hash-ref json/vals 'vote)]
                 [user-vote/cookie (track-per-user-vote req tweet-hash)])
            (when (<= (cadr user-vote/cookie) 2)
              (vote-tweet! client tweet-hash
                           #:upvote? (string=? vote "upvote")
                           #:feed-prefix feed-prefix))
            `(201 (,(car user-vote/cookie))
                  ,(redis-output->json
                    (get-items/redis
                     client
                     #:key "tweet-score:"
                     #:feed-prefix feed-prefix))))))

  (post "/vote/commits"
        (lambda (req)
          (let* ([json/vals (bytes->jsexpr (request-post-data/raw req))]
                 [commit-hash (hash-ref json/vals 'hash)]
                 [vote (hash-ref json/vals 'vote)]
                 [user-vote/cookie (track-per-user-vote req commit-hash)])
            (when (<= (cadr user-vote/cookie) 2)
              (vote-commit! client commit-hash
                          #:upvote? (string=? vote "upvote")
                          #:feed-prefix feed-prefix))
            `(201 (,(car user-vote/cookie))
                  ,(redis-output->json
                    (get-items/redis
                     client
                     #:key "commit-score:"
                     #:feed-prefix feed-prefix))))))

  (post "/vote/pubmed"
        (lambda (req)
          (let* ([json/vals (bytes->jsexpr (request-post-data/raw req))]
                 [hash (hash-ref json/vals 'hash)]
                 [vote (hash-ref json/vals 'vote)]
                 [user-vote/cookie (track-per-user-vote req hash)])
            (when (<= (cadr user-vote/cookie) 2)
              (vote-pubmed-article! client
                             (string-append feed-prefix hash)
                              #:upvote? (string=? vote "upvote")
                              #:feed-prefix feed-prefix))
            `(201 (,(car user-vote/cookie))
                  ,(redis-output->json
                    (get-items/redis
                     client
                     #:key "pubmed-score:"
                     #:feed-prefix feed-prefix))))))

  ;; TODO: use generic function generate voting endpoints
  (post "/vote/arxiv"
        (lambda (req)
          (let* ([json/vals (bytes->jsexpr (request-post-data/raw req))]
                 [hash (hash-ref json/vals 'hash)]
                 [vote (hash-ref json/vals 'vote)]
                 [user-vote/cookie (track-per-user-vote req hash)])
            (when (<= (cadr user-vote/cookie) 2)
              (vote-arxiv-article! client
                                   hash
                                   #:upvote? (string=? vote "upvote")
                                   #:feed-prefix feed-prefix))
            `(201 (,(car user-vote/cookie))
                  ,(redis-output->json
                    (get-items/redis
                     client
                     #:key "arxiv-score:"
                     #:feed-prefix feed-prefix))))))
  (displayln (string-append "Running the server on port " (number->string port)))

  (run #:port port #:log-file log-file))
