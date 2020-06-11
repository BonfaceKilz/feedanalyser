#lang racket

(require racket/date
         gregor
         redis)

(provide get-tweets
         store-tweet)

(define (store-tweet client tweet-hash)
  (let* [(vote-score 432)
         (tweet (car (hash-ref tweet-hash 'tweet)))
         (author (car (hash-ref tweet-hash 'author)))
         (timeposted (car (hash-ref tweet-hash 'timeposted)))
         (timeposted-in-seconds (->posix (parse-datetime timeposted  "yyyy-MM-dd HH:mm:ss")))
         (redis-tweet-key (string-append
                "tweet:"
                (number->string
                 (equal-hash-code tweet))))]
    (if (not (redis-has-key? client redis-tweet-key))
        (begin
          (redis-hash-set! client redis-tweet-key "author" (string->bytes/utf-8 author))
          (redis-hash-set! client redis-tweet-key "tweet" (string->bytes/utf-8 tweet))
          (redis-hash-set! client redis-tweet-key "timeposted" (string->bytes/utf-8 timeposted))
          (redis-zset-add!
           client
           "tweet-score:"
           redis-tweet-key
           (+ vote-score timeposted-in-seconds))
          (redis-zset-add!
           client
           "tweet-time:"
           redis-tweet-key
           timeposted-in-seconds))
        #f)))

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
    (remove* (list "\n" '()) tweets)
    ))

(define (get-tweets name #:number [number 10])
  (map
   (lambda (tweet)
     (let* [(ts (remove* (list "\n" '()) (string-split tweet "$@@$")))]
       (when (not (null? ts))
         (make-hash `((author ,(string-normalize-spaces (first ts)))
                      (tweet ,(string-normalize-spaces (second ts)))
                      (timeposted ,(string-normalize-spaces (third ts))))))))
   (get-raw-tweets name #:number number)))
