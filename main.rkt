#lang racket

(define-syntax-rule (reprovide e0 e ...)
  (begin
    (require e0 e ...)
    (provide (all-from-out e0 e ...))))

(reprovide
 "private/twitter.rkt"
 "private/github.rkt"
 "private/server.rkt"
 redis)