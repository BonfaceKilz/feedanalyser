#! /usr/bin/env racket
#lang racket

(require feedanalyser)

(define client (make-redis))

(start-server client #:port 8000)
