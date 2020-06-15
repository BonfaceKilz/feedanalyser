#! /usr/bin/env racket
#lang racket

(require feeds)

(define client (make-redis))

(start-server client #:port 8000)
