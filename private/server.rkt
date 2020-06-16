#lang racket

(require (planet dmac/spin)
         web-server/templates
         web-server/http
         json
         redis
         "twitter.rkt")

(provide start-server)

;; When starting the server, inject, the REDIS client
(define (start-server client #:port [port 8000] #:log-file [log-file "feed.log"])
  (get "/"
       (lambda (req)
         (let ([google-font-link "https://fonts.googleapis.com/css2?family=Amatic+SC&family=Josefin+Sans:ital,wght@1,300&display=swap"]
               [tweets (get-tweets/redis client)])
           (include-template "templates/polling.html"))))
  (post "/vote"
        (lambda (req)
          (let* ([json/vals (bytes->jsexpr (request-post-data/raw req))]
                 [tweet-hash (hash-ref json/vals 'tweet-hash)]
                 [upvote (hash-ref json/vals 'upvote)])
            (vote-tweet client tweet-hash #:upvote? (string=? upvote "upvote"))
            "OK")))
  (displayln (string-append "Running the server on port " (number->string port)))
  (run #:port port #:log-file log-file))