#lang racket

(require (planet dmac/spin)
         web-server/templates
         web-server/http
         web-server/http/cookie
         json
         redis
         "twitter.rkt"
         "github.rkt")

(provide start-server)

;; When starting the server, inject, the REDIS client
(define (start-server
         client
         #:port [port 8000]
         #:log-file [log-file "feed.log"]
         #:feed-prefix [feed-prefix ""])

  (define (create-cookie key val #:path [path "/"])
    (cookie->header (make-cookie
                     (string->bytes/utf-8 key)
                     val
                     #:secure? #t
                     #:path path)))
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
                 [vote (hash-ref json/vals 'vote)])
            (vote-tweet! client tweet-hash
                         #:upvote? (string=? vote "upvote")
                         #:feed-prefix feed-prefix)
            "OK")))

  (post "/vote/commits"
        (lambda (req)
          (let* ([json/vals (bytes->jsexpr (request-post-data/raw req))]
                 [commit-hash (hash-ref json/vals 'hash)]
                 [vote (hash-ref json/vals 'vote)])
            (vote-commit! client commit-hash
                          #:upvote? (string=? vote "upvote")
                          #:feed-prefix feed-prefix)
            "OK")))

  (displayln (string-append "Running the server on port " (number->string port)))

  (run #:port port #:log-file log-file))
