#lang racket

(require redis)


(provide remove-expired-keys!)


(define (remove-expired-keys! client zset-key-list)
  (let ([keys (redis-subzset
               client
               (car zset-key-list) ;; All elems in zset-key-list should have the same keys
               #:start 0
               #:stop -1)])
    (map (lambda (key)
           (unless (redis-has-key? client key)
               ;;; Remove expired tweets from the relevant zsets
             (for-each (lambda (zset-key)
                         (redis-zset-remove! client zset-key key))
                       zset-key-list)))
         keys)
    #t))
