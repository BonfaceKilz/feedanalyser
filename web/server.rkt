#lang racket

(require (planet dmac/spin)
         web-server/templates
         web-server/http
         web-server/http/cookie
         web-server/http/cookie-parse
         json
         redis
         "twitter.rkt"
         "github.rkt")

(provide start-server)


(define (extract-cookie req cookie-key)
  (define cookies (request-cookies req))
  (define cookie-find
    (findf (lambda (c)
             (string=? cookie-key (client-cookie-name c)))
           cookies))
  (if cookie-find
      cookie-find
      #f))

(define (create-cookie key val #:path [path "/"])
    (cookie->header (make-cookie
                     key
                     val
                     #:secure? #t
                     #:path path)))

(define (track-per-user-vote req hash)
  (let* ([hash-cookie (string-replace
                       hash ":" "--")]
         [cookie (extract-cookie req hash-cookie)]
         [cookie-hash-value
                  (number->string
                   (if
                    cookie
                    (+ (string->number
                        (client-cookie-value cookie)) 1) 1))])
    (cons
     (create-cookie hash-cookie cookie-hash-value)
     (string->number cookie-hash-value))))

;; When starting the server, inject, the REDIS client
(define (start-server
         client
         #:port [port 8000]
         #:log-file [log-file "feed.log"]
         #:feed-prefix [feed-prefix ""])

  (get "/"
       (lambda (req)
         (let ([tweets/time (get-tweets/redis client #:key "tweet-time:" #:feed-prefix feed-prefix)]
               [commits (get-commits/redis client #:feed-prefix feed-prefix)]
               [tweets/score
                (get-tweets/redis
                 client #:key "tweet-score:" #:feed-prefix feed-prefix)])
           (include-template "templates/voting.html"))))

  (post "/update-cookies"
        (lambda (req)
          (let* ([json/vals (bytes->jsexpr (request-post-data/raw req))]
                 [tweet-order (hash-ref json/vals 'tweet-order)]
                 [tweet-select-by (hash-ref json/vals 'tweet-select-by)]
                 [commit-order (hash-ref json/vals 'commit-order)]
                 [commit-select-by (hash-ref json/vals 'commit-select-by)]
                 [cookies-list
                  (map (lambda (el)
                         (create-cookie (car el) (cadr el)))
                       (list `("tweet-order" ,tweet-order)
                             `("tweet-selected-by" ,tweet-select-by)
                             `("commit-order" ,commit-order)
                             `("commit-select-by" ,commit-select-by)))])
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
            `(201 (,(car user-vote/cookie)) "OK"))))

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
            `(201 (,(car user-vote/cookie)) "OK"))))

  (displayln (string-append "Running the server on port " (number->string port)))

  (run #:port port #:log-file log-file))
