#lang racket

(require gregor
         racket/date
         redis)


(provide get-tweets/redis
         get-tweets/twitter
         store-tweet
         store-multiple-tweets
         vote-tweet
         (struct-out feed-tweet))


; A simple (placeholder) tweet type, that contains metadata about a tweet
(struct feed-tweet (author content timeposted hash) #:transparent)

(define (remove-expired-tweets-from-zsets client)
  (let ([tweet-scores (redis-subzset
                       client
                       "tweet-score:"
                       #:start 0
                       #:stop -1)])
    (map (lambda (tweet-hash)
           (unless (redis-has-key? client tweet-hash)
               ;;; Remove expired tweets from the relevant zsets
             (begin
               (redis-zset-remove! client "tweet-score:" tweet-hash)
               (redis-zset-remove! client "tweet-time:" tweet-hash))
             (redis-hash-get client tweet-hash)))
         tweet-scores)))


(define (store-tweet client tweet-hash)
  "Store tweets to REDIS. The tweets expire after 1 month"
  (let* [(vote-score 0)
         (tweet (car (hash-ref tweet-hash 'tweet)))
         (author (car (hash-ref tweet-hash 'author)))
         (timeposted (car (hash-ref tweet-hash 'timeposted)))
         (timeposted-in-seconds (->posix (parse-datetime timeposted  "yyyy-MM-dd HH:mm:ss")))
         (redis-tweet-key (string-append
                "tweet:"
                (number->string
                 (equal-hash-code tweet))))]

    ;; Remove any tweets that had expired from the zsets
    (remove-expired-tweets-from-zsets client)

    (cond
     [(not (redis-has-key? client redis-tweet-key))
      (redis-hash-set! client redis-tweet-key "author" (string->bytes/utf-8 author))
      (redis-hash-set! client redis-tweet-key "tweet" (string->bytes/utf-8 tweet))
      (redis-hash-set! client redis-tweet-key "hash" redis-tweet-key)
      (redis-hash-set! client redis-tweet-key "timeposted" (string->bytes/utf-8 timeposted))
      (redis-hash-set! client redis-tweet-key "score" (number->string vote-score))
      (redis-zset-add!
       client
       "tweet-score:"
       redis-tweet-key
       vote-score)
      (redis-zset-add!
       client
       "tweet-time:"
       redis-tweet-key
       timeposted-in-seconds)
      ;; Expire tweets after 1 week
      (redis-expire-in! client redis-tweet-key (* 30 7 24 60 60 100))]
     [else #f])))


(define (get-raw-tweets name #:number [number 10])
  (let* [(n (if (string? number)
               number
               (number->string number)))
        (tweets (string-split
                 (with-output-to-string
                   (lambda ()
                     (system
                      (string-append
                       "twint -s "
                       name
                       " --limit "
                       n
                       " --link include"
                       " --format '{username} $@@$ {tweet} $@@$ {date} {time}||||||'"))))
                 "||||||"))]
    (remove* (list "\n" '()) tweets)))


(define (get-tweets/twitter name #:number [number 10])
  "Get tweets from Twitter"
  (map
   (lambda (tweet)
     (let* [(ts (remove* (list "\n" '()) (string-split tweet "$@@$")))]
       (when (not (null? ts))
         (make-hash `((author ,(string-normalize-spaces (first ts)))
                      (tweet ,(string-normalize-spaces (second ts)))
                      (timeposted ,(string-normalize-spaces (third ts))))))))
   (get-raw-tweets name #:number number)))


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


(define (store-multiple-tweets client name)
  (for-each (lambda (tweet)
              (store-tweet client tweet))
            (get-tweets/twitter name)))
