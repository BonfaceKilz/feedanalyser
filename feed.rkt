#! /usr/bin/env racket
#lang racket

(require "feeds/twitter.rkt")
(require "feeds/github.rkt")

;; Define the default user and action
(define twitter-user (make-parameter "pjotrprins"))
(define action (make-parameter "display"))

(define parser
  (command-line
   #:usage-help
   "Print out a user's tweets by:"
   "    ./feed -u <user-name> -a display"
   "Store the tweets to a redis instance:"
   "    ./feed -u <user-name> -a store"

   #:once-each
   [("-u" "--user") user
                    "The twitter handle of the user"
                    (twitter-user user)]
   [("-a" "--action") user-action
                      "Either store the tweets to Redis or display them"
                      (action user-action)]
   #:args () (void)))

(cond
  [(equal? (action) "store") (store-tweets (twitter-user)
                                           (has-words?
                                            '("corona" "covid" "crisis")))]
  [(equal? (action) "display") (display-tweets (twitter-user)
                                               (has-words?
                                                '("corona" "covid" "crisis")))]
  [else (display "Please perform the correct action")])
