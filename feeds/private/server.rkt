#lang racket

(require (planet dmac/spin)
         web-server/templates
         web-server/http
         redis
         "twitter.rkt")

(provide start-server)

;; TODO: Get configs from file
(define c (make-redis))

(get "/"
     (lambda (req)
       (let ([google-font-link "https://fonts.googleapis.com/css2?family=Amatic+SC&family=Josefin+Sans:ital,wght@1,300&display=swap"]
             [tweets (get-tweets/redis c)])
         (include-template "templates/polling.html"))))

(post "/vote"
      (lambda (req)
        (let* ([json/vals (bytes->jsexpr (request-post-data/raw req))]
               [tweet-hash (hash-ref json/vals 'tweet-hash)]
               [upvote (hash-ref json/vals 'upvote)])
          (vote-tweet c tweet-hash #:upvote? (string=? upvote "upvote"))
          "OK"
          )
        ))

(define start-server
  (lambda()
    (run)))
