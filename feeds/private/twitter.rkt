#lang racket

(provide get-tweets)

;;; TODO Replace later
(define (store-tweets name filter-fn)
  """Store tweets to a list"
  (let [(c (make-redis))]
    (for-each (lambda (tweet)
                (redis-list-append!
                 c
                 name
                 (string->bytes/utf-8 (tweet->json tweet))))
              (filter filter-fn
                      (get-tweets name)))))

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
