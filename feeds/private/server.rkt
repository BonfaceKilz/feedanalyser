#lang racket

(require (planet dmac/spin)
         web-server/templates
         redis
         "twitter.rkt")

(provide start-server)

;; TODO: Get configs from file
(define c (make-redis))

(get "/"
     (lambda (req)
       (let ([name (params req 'name)]
             [google-font-link "https://fonts.googleapis.com/css2?family=Amatic+SC&family=Josefin+Sans:ital,wght@1,300&display=swap"]
             [tweets (get-tweets/redis c)])
         (include-template "templates/polling.html"))))

(post "/vote"
      (lambda (req)
        (let* ([votetype (params req 'votetype)]
               [tweet-hash (params req 'tweet-hash)]
               [upvote? (string=? votetype "upvote")])
          (vote-tweet c tweet-hash #:upvote? upvote?)
          (include-template "templates/polling.html"))))

(define start-server
  (lambda()
    (run)))
