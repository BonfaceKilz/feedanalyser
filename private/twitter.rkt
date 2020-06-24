#lang racket

(require gregor
         racket/date
         redis)


(provide get-tweets/redis
         get-tweets/twitter
         store-tweets!
         vote-tweet!
         remove-expired-tweets!
         remove-all-tweets!
         (struct-out feed-tweet))


; A simple (placeholder) tweet type, that contains metadata about a tweet
(struct feed-tweet (author content timeposted hash) #:transparent)


;; Check for tweets that have expired and remove them
(define (remove-expired-tweets! client)
  (let ([keys (redis-subzset
               client
               "tweet-score:"
               #:start 0
               #:stop -1)])
    (map (lambda (key)
           (unless (redis-has-key? client key)
               ;;; Remove expired tweets from the relevant zsets
             (begin
               (redis-zset-remove! client "tweet-score:" key)
               (redis-zset-remove! client "tweet-time:" key))
             (redis-hash-get client key)))
         keys)))


(define (get-raw-tweets userlist #:search-terms [search-terms #f] #:number [number 10])
  (let* [(limit-n (if (string? number)
               number
               (number->string number)))
        (tweets (string-split
                 (with-output-to-string
                   (lambda ()
                     (system
                      (string-append
                       "twint --userlist '"
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
                       " --format '{username} $@@$ {tweet} $@@$ {date} {time}||||||'"))))
                 "||||||"))]
    (remove* (list "\n" '()) tweets)))


;; Get tweets from twitter and return them as strings. Search terms are search
;; words that can used within twitter's own advanced search. `userlist' is a
;; comma-separated list of users. It can be a single user though. Valid
;; examples: "bonfacekilz"  "bonfacekilz,genenetwork2"
(define (get-tweets/twitter userlist #:search-terms [search-terms #f] #:number [number 10])
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
         (define hash (number->string
                       (equal-hash-code content)))
         (feed-tweet author content timeposted hash))))
   (get-raw-tweets userlist #:search-terms search-terms #:number number)))


(define (get-tweets/redis
         client
         #:key [key "tweet-score:"]
         #:start [start 0]
         #:stop [stop -1]
         #:reverse? [reverse? #t])
  "Get tweets from REDIS. Also removes expired tweets from the zsets"
  (let ([tweet-scores (redis-subzset
                       client
                       key
                       #:start start
                       #:stop stop
                       #:reverse? reverse?)])
    (map (lambda (tweet-hash)
           (redis-hash-get client tweet-hash))
         tweet-scores)))


(define (vote-tweet! client tweet-hash #:upvote? [upvote #t])
  (let* [(n (if upvote 1 -1))
         (key "tweet-score:")
         (score (string->number (bytes->string/utf-8
                                 (redis-hash-ref client tweet-hash "score"))))]
    (begin
      (redis-hash-set! client tweet-hash "score"
                       (number->string (+ score n)))
      (redis-zset-incr! client key tweet-hash (* n 1000)))))


;; Given a list of feed-tweets, store them in REDIS
(define (store-tweets! client tweets)
  (define (store-tweet! c tweet)
    "Store tweets to REDIS. The tweets expire after 1 month"
    (let* [(serialized-tweet (serialize-tweet tweet))
           (author (feed-tweet-author serialized-tweet))
           (content (feed-tweet-content serialized-tweet))
           (timeposted (feed-tweet-timeposted serialized-tweet))]
           (hash (feed-tweet-hash tweet))
           (key (string-append
                 "tweet:"
                 hash))
      (cond
       [(not (redis-has-key? c key))
        (redis-hash-set! c key "author" author)
        (redis-hash-set! c key "tweet" content)
        (redis-hash-set! c key "hash" key)
        (redis-hash-set! c key "score" "0")
        (redis-zset-add!
         c
         "tweet-score:"
         key
         0)
        (redis-zset-add!
         c
         "tweet-time:"
         key
         timeposted)
        ;; Expire tweets after 1 week
        (redis-expire-in! c key (* 30 7 24 60 60 100))]
       [else #f])))
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
              (string->bytes/utf-8 (feed-tweet-timeposted tweet))
              (string->bytes/utf-8 (feed-tweet-hash tweet))))

;; Remove all tweets
(define (remove-all-tweets! client)
  (let ([keys (redis-subzset
               client
               "tweet-score:"
               #:start 0
               #:stop -1)])
    (map (lambda (key)
           (redis-zset-remove! client "tweet-score:" key)
           (redis-zset-remove! client "tweet-time:" key)
           (redis-remove! client key)
           #t)
         keys)))
