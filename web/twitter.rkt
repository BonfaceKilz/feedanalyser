#lang racket

(require gregor
         racket/date
         redis
         "votes.rkt")


(provide get-tweets/redis
         get-tweets/twitter
         store-tweets!
         vote-tweet!
         remove-expired-tweets!
         remove-all-tweets!
         (struct-out feed-tweet))


; A simple (placeholder) tweet type, that contains metadata about a tweet
(struct feed-tweet (author content timeposted hash replies retweets likes url) #:transparent)


;; Check for tweets that have expired and remove them
(define (remove-expired-tweets! client #:feed-prefix [feed-prefix ""])
  (remove-expired-keys! client (list
                                (string-append feed-prefix "tweet-score:")
                                "tweet-time:")))


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
  (map
   (lambda (tweet)
     (let* [(ts (remove* (list "\n" '()) (string-split tweet "$@@$")))]
       (when (not (null? ts))
         (define author (string-normalize-spaces (first ts)))
         (define content (string-normalize-spaces (second ts)))
         (define timeposted (->posix
                             (parse-datetime
                              (string-normalize-spaces (third ts))
                              "yyyy-MM-dd HH:mm:ss")))
         (define replies (fourth ts))
         (define retweets (fifth ts))
         (define likes (sixth ts))
         (define url (seventh ts))
         (define hash (number->string
                       (equal-hash-code content)))
         (feed-tweet author content timeposted hash replies retweets likes url))))
   (get-raw-tweets userlist #:search-terms search-terms #:number number)))


(define (get-tweets/redis
         client
         #:key [key "tweet-score:"]
         #:start [start 0]
         #:stop [stop -1]
         #:reverse? [reverse? #t]
         #:feed-prefix [feed-prefix ""])
  (let ([tweet-scores (redis-subzset
                       client
                       (string-append feed-prefix key)
                       #:start start
                       #:stop stop
                       #:reverse? reverse?)])
    (map (lambda (tweet-hash)
           (redis-hash-get client tweet-hash))
         tweet-scores)))


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
           (tz-name (date*-time-zone-name tweet-date*))]
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
                         (string-append
                          (date->string tweet-date* #t) ;; set #t to show the time too
                          " " tz-name))
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
        (redis-expire-in! c key (* 14 24 60 60 100))]
       [else  ;; Update the tweet metrics
        (redis-hash-set! c key "replies" replies)
        (redis-hash-set! c key "retweets" retweets)
        (redis-hash-set! c key "likes" likes)])))
  (cond
   [(not (null? tweets))
    (for-each (lambda (tweet)
                (store-tweet! client tweet))
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
