#lang racket/base

#| Worker for fetching posts from twitter

Run the worker after every 60 minutes vis-a-vis an infinite loop

|#

(require feeds)

(define client (make-redis))

(define hashtag/username "covid_19_bh20")

(define (hours->seconds hours) (* hours 60 60))

(void
 (thread
  (lambda _
    (let loop ()
      (sleep 10)
      (loop)))))

(let loop ()
  (displayln "Adding tweets:")
  (store-multiple-tweets client hashtag/username)
  (sleep (hours->seconds 3))
  (loop))
