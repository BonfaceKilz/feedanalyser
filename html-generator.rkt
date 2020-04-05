#! /usr/bin/env racket
#lang racket

(require web-server/templates)
(require redis)
(require json)

(define (fast-template tweets)
  (include-template "templates/feed.html"))

(define (redis/read name)
  (let* [(c (make-redis))
         (tweets (redis-sublist c name #:start 0 #:stop -1))]
    (define (iter tweets)
      (let [(tweet (redis-list-pop-left! c name))]
        (if (false? tweet)
            tweets
            (iter (cons (bytes->jsexpr tweet) tweets))
            )
        )
      )
    (iter '())))

(if (directory-exists? "build")
    (when (file-exists? '"build/index.html")
      (delete-file '"build/inpdex.html"))
    (make-directory "build"))

(define (generate-html-file)
  (call-with-output-file "build/index.html"
    (lambda (out)
      (let [(tweets (redis/read "AbePalmer"))]
        (if (empty? tweets)
            'nil
            (write-string (fast-template tweets) out))))))

(generate-html-file)
