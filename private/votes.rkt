#lang racket

(require redis)


(provide remove-expired-keys!
         remove-all-keys!
         vote!)


(define (remove-expired-keys! client zset-keys/list)
  (let ([keys (redis-subzset
               client
               (car zset-keys/list) ;; All elems in zset-keys/list should have the same keys
               #:start 0
               #:stop -1)])
    (map (lambda (key)
           (unless (redis-has-key? client key)
               ;;; Remove expired tweets from the relevant zsets
             (for-each (lambda (zset-key)
                         (redis-zset-remove! client zset-key key))
                       zset-keys/list)))
         keys)
    #t))


(define (remove-all-keys! client key/regexp)
  ;; Remove any stale tweets
  (map (lambda (key)
         (redis-remove! client key))
       (redis-keys client key/regexp))
  #t)


(define (vote! client zset-key hash #:upvote? [upvote #t])
  (let* [(n (if upvote 1 -1))
         (score (string->number (bytes->string/utf-8
                                 (redis-hash-ref client hash "score"))))]
    (begin
      (redis-hash-set! client hash "score"
                       (number->string (+ score n)))
      (redis-zset-incr! client zset-key hash (* n 1000)))))
