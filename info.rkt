#lang setup/infotab

(define name "feedanalyser")
(define blurb
  (list '(p "Fetch feeds from different places in Racket")))
(define primary-file "main.rkt")
(define categories '(net))
(define version "0.0.0")
(define deps '("base"
               "redis-rkt"
               "gregor"
               "https://github.com/dmac/spin.git"
               "web-server-lib"))
(define build-deps '())

