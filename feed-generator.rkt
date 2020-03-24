#! /usr/bin/env racket
#lang racket

(require web-server/templates)
(require redis)
(require json)

(define (fast-template tweets)
  (include-template "templates/feed.html"))


;; For now just read everything from a the queue on Redis
(define (redis/read name)
  (let* [(c (make-redis))
         (tweets (redis-sublist c name #:start 0 #:stop -1))]
    (map bytes->jsexpr tweets)
    ))

;; For now just display the HTML. You could do other things
;; with it
(display (fast-template (redis/read "AbePalmer")))
