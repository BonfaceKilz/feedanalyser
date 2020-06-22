#lang racket

(require gregor
         racket/date
         redis)


(provide get-tweets/redis
         get-tweets/twitter
         store-tweets
         vote-tweet
         remove-expired-tweets
         (struct-out feed-tweet))


; A simple (placeholder) tweet type, that contains metadata about a tweet
(struct feed-tweet (author content timeposted hash) #:transparent)


;; Check for tweets that have expired and remove them
(define (remove-expired-tweets client)
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
  (let* [(n (if (string? number)
               number
               (number->string number)))
        (tweets (string-split
                 (with-output-to-string
                   (lambda ()
                     (system
                      (string-append
                       "twint --userlist "
                       userlist
                       (cond
                        [search-terms
                         (string-append " -s"
                                        search-terms)]
                        [else ""])
                       " --limit "
                       n
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
         (define hash (string-append
                       "tweet:"
                       (number->string
                        (equal-hash-code content))))
         (feed-tweet author content timeposted hash))))
   (get-raw-tweets userlist search-terms #:number number)))


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


(define (vote-tweet client tweet-hash #:upvote? [upvote #t])
  (let* [(n (if upvote 1 -1))
         (key "tweet-score:")
         (score (string->number (bytes->string/utf-8
                                 (redis-hash-ref client tweet-hash "score"))))]
    (begin
      (redis-hash-set! client tweet-hash "score"
                       (number->string (+ score n)))
      (redis-zset-incr! client key tweet-hash (* n 1000)))))


;; Given a list of feed-tweets, store them in REDIS
(define (store-tweets client tweets)
  (define (store-tweet c tweet)
    "Store tweets to REDIS. The tweets expire after 1 month"
    (let* [(serialized-tweet (serialize-tweet tweet))
           (vote-score 0)
           (author (feed-tweet-author serialized-tweet))
           (content (feed-tweet-content serialized-tweet))
           (redis-tweet-key (feed-tweet-hash serialized-tweet))
           (timeposted (feed-tweet-timeposted serialized-tweet))]
      (cond
       [(not (redis-has-key? c (feed-tweet-hash serialized-tweet)))
        (redis-hash-set! c redis-tweet-key "author" author)
        (redis-hash-set! c redis-tweet-key "tweet" content)
        (redis-hash-set! c redis-tweet-key "hash" redis-tweet-key)
        (redis-hash-set! c redis-tweet-key "timeposted" timeposted)
        (redis-hash-set! c redis-tweet-key "score" (number->string vote-score))
        (redis-zset-add!
         c
         "tweet-score:"
         redis-tweet-key
         vote-score)
        (redis-zset-add!
         c
         "tweet-time:"
         redis-tweet-key
         timeposted-in-seconds)
        ;; Expire tweets after 1 week
        (redis-expire-in! c redis-tweet-key (* 30 7 24 60 60 100))]
       [else #f])))
  (cond
   [(and (list? tweets) (not (null? tweets)))
    (for-each (lambda (tweet)
                (store-tweet client tweet))
              (get-tweets/twitter name))
    #t]
   [(not (null? tweets)) ;; Only one tweet
    (store-tweet client tweets)
    #t]
   [else #f]))


;; Serialize a tweet into a form that can be stored in REDIS
(define (serialize-tweet tweet)
  (feed-tweet (string->bytes/utf-8 (feed-tweet-author tweet))
              (string->bytes/utf-8 (feed-tweet-content tweet))
              (string->bytes/utf-8 (feed-tweet-timeposted tweet))
              (string->bytes/utf-8 (feed-tweet-hash tweet))))
