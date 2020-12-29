#lang racket

(require gregor
         racket/date
         redis
         threading
         "votes.rkt")


(provide get-tweets/twitter
         store-tweets!
         vote-tweet!
         remove-all-tweets!
         (struct-out feed-tweet))


; A simple (placeholder) tweet type, that contains metadata about a tweet
(struct feed-tweet (author content timeposted hash replies retweets likes url) #:transparent)


;; Remove all tweets from Redis
(define (remove-all-tweets! client #:feed-prefix [feed-prefix ""])
  (remove-all-keys! client
                    (string-append feed-prefix "tweet*")))


(define (get-raw-tweets userlist
                        #:search-terms [search-terms #f]
                        #:number [number 10]
                        #:min-retweets [min-retweets 2]
                        #:since [since
                                 (format (~t (-weeks (today) 2) "y-M-d"))])
  (let* [(limit-n (if (string? number)
               number
               (number->string number)))
        (tweets (string-split
                 (with-output-to-string
                   (lambda ()
                     (system
                      (string-append
                       "twint --stats --userlist '"
                       userlist
                       "'"
                       (cond
                        [search-terms
                         (string-append " -s '"
                                        search-terms
                                        "'")]
                        [else ""])
                       " --limit "
                       limit-n
                       " --link include"
                       (when since
                         (string-append
                          " --since '"
                          since
                          "'"))
                       (when min-retweets
                         (string-append
                          " --min-retweets "
                          (if (number? min-retweets)
                              (number->string min-retweets)
                              min-retweets)))
                       " --format '{username} $@@$ {tweet} $@@$ {date} {time} $@@$ {replies} $@@$ {retweets} $@@$ {likes} $@@$ {link}||||||'"))))
                 "||||||"))]
    (remove* (list "\n" '()) tweets)))


;; Get tweets from twitter and return them as strings. Search terms are search
;; words that can used within twitter's own advanced search. `userlist' is a
;; comma-separated list of users. It can be a single user though. Valid
;; examples: "bonfacekilz"  "bonfacekilz,genenetwork2"
(define (get-tweets/twitter userlist #:search-terms [search-terms #f]
                            #:number [number 10]
                            #:min-retweets [min-retweets 20]
                            #:since [since
                                     (format
                                      (~t (-weeks (today) 2) "y-M-d"))])
  "Get tweets from Twitter"
  (let [(users (string-split userlist ","))]
    (if (< (length users) 20)
        (unless (empty? users)
          (map
           (lambda (tweet)
             (define ts
               (if (string-contains? tweet "Scraping will stop now")
                   '()
                   (remove* (list "\n" '())
                            (string-split tweet "$@@$"))))
             (unless (null? ts)
               (define author (~> ts first string-normalize-spaces))
               (define content (~> ts second string-normalize-spaces))
               (define timeposted (->posix
                                   (parse-datetime
                                    (~> ts third string-normalize-spaces)
                                    "yyyy-MM-dd HH:mm:ss")))
               (define replies (~> ts fourth string-normalize-spaces))
               (define retweets (~> ts fifth string-normalize-spaces))
               (define likes (~> ts sixth string-normalize-spaces))
               (define hash (~> content equal-hash-code number->string))
               (define url (~> ts seventh string-normalize-spaces))
               (feed-tweet author content timeposted hash replies retweets likes url)))
           (get-raw-tweets userlist
                           #:search-terms search-terms
                           #:number number
                           #:min-retweets min-retweets
                           #:since since)))
        (let-values ([(users-a users-b)
                      (split-at
                       users
                       (truncate (/ (length users) 2)))])
          (append
           (get-tweets/twitter (string-join users-a ",")
                              #:search-terms search-terms
                              #:number number
                              #:min-retweets min-retweets
                              #:since since)
           (get-tweets/twitter (string-join users-b ",")
                              #:search-terms search-terms
                              #:number number
                              #:min-retweets min-retweets
                              #:since since))))))


(define (vote-tweet! client key
                     #:upvote? [upvote? #t]
                     #:feed-prefix [feed-prefix ""])
  (vote! client (string-append feed-prefix "tweet-score:")
         key #:upvote? upvote?))


;; Given a list of feed-tweets, store them in REDIS
(define (store-tweets! client tweets #:feed-prefix [feed-prefix ""])
  (define (store-tweet! c tweet)
    "Store tweets to REDIS. The tweets expire after 1 month"
    (let* [(serialized-tweet (serialize-tweet tweet))
           (author (feed-tweet-author serialized-tweet))
           (content (feed-tweet-content serialized-tweet))
           (hash (feed-tweet-hash tweet))
           (key (string-append
                 feed-prefix
                 "tweet:"
                 hash))
           (timeposted (feed-tweet-timeposted tweet))
           (replies (feed-tweet-replies tweet))
           (retweets (feed-tweet-retweets tweet))
           (likes (feed-tweet-likes tweet))
           (url (feed-tweet-url tweet))
           (tweet-date* (seconds->date timeposted))
           (expiry/seconds (+ (* (date->seconds tweet-date*) 1000)
                              (* 14 24 60 60 1000)))
           (tz-name (date*-time-zone-name tweet-date*))]
      (unless (negative? (- expiry/seconds
                          (current-milliseconds))) ;; Don't add expired tweets
        (cond
         [(not (redis-has-key? c key))
          (redis-hash-set! c key "author" author)
          (redis-hash-set! c key "tweet" content)
          (redis-hash-set! c key "hash" key)
          (redis-hash-set! c key "replies" replies)
          (redis-hash-set! c key "retweets" retweets)
          (redis-hash-set! c key "likes" likes)
          (redis-hash-set! c key "url" url)
          (redis-hash-set! c key "timeposted"
                           (parameterize ([date-display-format 'iso-8601])
                             (string-append
                              ;; set #t to show the time too
                              (date->string tweet-date* #t)
                              " " tz-name)))
          (redis-hash-set! c key "score" "0")
          (redis-zset-add!
           c
           (string-append feed-prefix "tweet-score:")
           key
           (/ timeposted 1000000000.0))
          (redis-zset-add!
           c
           (string-append feed-prefix "tweet-time:")
           key
           timeposted)
          ;; Expire tweets after 2 weeks
          (redis-expire-at! c key expiry/seconds)]
         [else  ;; Update the tweet metrics
          (redis-hash-set! c key "replies" replies)
          (redis-hash-set! c key "retweets" retweets)
          (redis-hash-set! c key "likes" likes)]))))
  (cond
   [(not (null? tweets))
    (for-each (lambda (tweet)
                (unless (or (null? tweet)
                            (void? tweet)
                            (not (feed-tweet? tweet)))
                  (store-tweet! client tweet)))
              (if (list? tweets)
                  tweets
                  `(,tweets)))
    #t]
   [else #f]))


;; Serialize a tweet into a form that can be stored in REDIS
(define (serialize-tweet tweet)
  (feed-tweet (string->bytes/utf-8 (feed-tweet-author tweet))
              (string->bytes/utf-8 (feed-tweet-content tweet))
              (string->bytes/utf-8 (number->string
                                    (feed-tweet-timeposted tweet)))
              (string->bytes/utf-8 (feed-tweet-hash tweet))
              (string->bytes/utf-8 (feed-tweet-replies tweet))
              (string->bytes/utf-8 (feed-tweet-retweets tweet))
              (string->bytes/utf-8 (feed-tweet-likes tweet))
              (string->bytes/utf-8 (feed-tweet-url tweet))))
