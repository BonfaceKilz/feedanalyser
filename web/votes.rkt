#lang racket

(require redis
         threading)


(provide remove-expired-keys!
         remove-all-keys!
         vote!)


(define (remove-expired-keys! client zset-keys/list)
  (define keys
    (~> (map (lambda (zset-key)
                  (redis-subzset
                   client
                   zset-key
                   #:start 0
                   #:stop -1))
             zset-keys/list)
        flatten
        remove-duplicates))
  (map (lambda (key)
         (unless (redis-has-key? client key)
           ;; Remove expired tweets from the relevant zsets
           (for-each (lambda (zset-key)
                       (redis-zset-remove! client zset-key key))
                     zset-keys/list)))
       keys)
  #t)


(define (remove-all-keys! client key/regexp)
  ;; Remove any stale tweets
  (map (lambda (key)
         (redis-remove! client key))
       (redis-keys client key/regexp))
  #t)


(define (vote! client zset-key hash #:upvote? [upvote? #t])
  (let* [(score (~> (redis-hash-ref client hash "score")
                    bytes->string/utf-8
                    string->number))]
    (begin
      ;; Set score
      (redis-hash-set! client hash "score"
                       (~> score
                           ((if upvote? + -) 1)
                           number->string))
      ;; Increase zscore by 1000
      (redis-zset-incr! client zset-key hash
                        (if upvote?
                            1000
                            -1000)))))
