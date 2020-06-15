#lang info

(define version "0.0.0")
(define name "feeds")
(define blurb
  (list '(p "Fetch feeds from different places in Racket")))
(define primary-file "main.rkt")
(define categories '(net))
(define deps '("base"
               "redis-rkt"
               "gregor"
               "https://github.com/dmac/spin.git"
               "web-server-lib"))
(define build-deps '())
