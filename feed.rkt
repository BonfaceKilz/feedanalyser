#! /usr/bin/env racket
#lang racket

(require web-server/templates)
(require redis)
(require json)

(require "feeds/twitter.rkt")
(require "feeds/github.rkt")

;; Define the default user and action
(define twitter-user (make-parameter "pjotrprins"))
(define action (make-parameter "display"))

(define (redis/read name)
  (let* [(c (make-redis))]
    (redis-list-get c name)))

(if (directory-exists? "build")
    (when (file-exists? '"build/index.html")
      (delete-file '"build/index.html"))
    (make-directory "build"))

(define (generate-html-file)
  (define (fast-template tweets commits)
    (include-template "templates/feed.html"))
  (call-with-output-file "build/index.html"
    (lambda (out)
      (let [(tweets (redis/read "BioHackanthonTweets"))
            (commits (redis/read "Github"))]
        (when (and tweets commits)
          (write-string (fast-template
                         (reverse tweets)
                         (map bytes->jsexpr commits)) out))))))


(store-gh-commits "Schematize")
(generate-html-file)
