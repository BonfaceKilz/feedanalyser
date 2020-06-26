#lang racket

(require redis)


(provide remove-expired-keys!
         remove-all-keys!)


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
