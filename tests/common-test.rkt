#lang racket/base

(require rackunit
         threading
         "../web/common.rkt"
         "../web/pubmed.rkt")

(let ([test-feed-struct (feed-pubmed "a" "b" "c" "d" "e" "f")])
  (check-equal?
   (serialize-struct feed-pubmed test-feed-struct)
   (feed-pubmed #"a" #"b" #"c" #"d" #"e" #"f")))

(check-equal?
 (~> "<foo>Hell<bar>o, world!</bar></foo>"
     open-input-string
     remove-markup)
 "Hello, world!")
