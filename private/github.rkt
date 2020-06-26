#lang racket

(require json
         lens/common
         lens/data/hash
         gregor
         redis
         simple-http
         "votes.rkt")


(provide get-commits/github
         get-commits/redis
         store-commits!
         remove-expired-commits!
         remove-all-commits!
         vote-commit!
         (struct-out feed-commit))


; A struct type to store details about commits from various places
(struct feed-commit (author repository content timeposted hash url repository-url) #:transparent)

;; Check for tweets that have expired and remove them
(define (remove-expired-commits! client)
  (remove-expired-keys! client (list "commit-score:" "commit-time:")))


;; Remove all commits
(define (remove-all-commits! client)
  (remove-all-keys! client "commit*"))


;; Get tweets from github, storing them in a struct
(define (get-commits/github username reponame #:page [page 1] #:per-page [per-page 10])
  (define (->struct commit)
    (let* [(commit-dict (hash-ref commit 'commit))
           (author-dict (hash-ref commit-dict 'committer))
           (author (hash-ref author-dict 'name))
           (content (hash-ref commit-dict 'message))
           (timeposted (hash-ref author-dict 'date))
           (url (hash-ref commit 'html_url))
           (repository-url (string->bytes/utf-8
                            (string-append
                             "https://github.com/"
                             username
                             "/"
                             reponame)))
           (hash (hash-ref commit 'sha))]
      (feed-commit author reponame content timeposted hash url repository-url)))

  (let* ([requester (update-ssl (update-host json-requester "api.github.com") #t)]
         [params `((page . ,(number->string page)) (per_page . ,(number->string per-page)))]
         [commits (json-response-body
                   (get requester
                        (string-append
                         "/repos/"
                         username
                         "/"
                         reponame
                         "/commits")
                        #:params params))])
    (map
     (lambda (commit)
       (->struct commit))
     commits)))


(define (get-commits/redis
         client
         #:key [key "commit-time:"]
         #:start [start 0]
         #:stop [stop -1]
         #:reverse? [reverse? #t])
  (map (lambda (commit/key)
         (redis-hash-get client commit/key))
       (redis-subzset
        client
        key
        #:start start
        #:stop stop
        #:reverse? reverse?)))


(define (store-commits! client commits)
  (define (store-commit! c commit*)
    (let [(key (string-append
                "commit:"
                (feed-commit-hash commit*)))
          (timeposted/seconds (->posix
                               (iso8601->datetime
                                (feed-commit-timeposted commit*))))]
      (cond
       [(not (redis-has-key? c key))
        (redis-hash-set! c key "author" (feed-commit-author commit*))
        (redis-hash-set! c key "content" (feed-commit-content commit*))
        (redis-hash-set! c key "repository" (feed-commit-repository commit*))
        (redis-hash-set! c key "repository-url" (feed-commit-repository-url commit*))
        (redis-hash-set! c key "timeposted" (feed-commit-timeposted commit*))
        (redis-hash-set! c key "url" (feed-commit-url commit*))
        (redis-hash-set! c key "hash" (feed-commit-hash commit*))
        (redis-zset-add! c "commit-time:" key timeposted/seconds)
        (redis-expire-in! c key (* 7 24 60 60 100))
        (redis-zset-add! c "score:" key 0)])))
  (cond
   [(not (null? commits))
    (for-each (lambda (commit*)
                (store-commit! client commit*))
              (if (list? commits)
                  commits
                  `(,commits)))]))

(define (vote-commit! client key #:upvote? [upvote #t])
  (vote! client "commit-score:" key #:upvote upvote))
