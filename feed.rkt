#! /usr/bin/env racket
#lang racket

(require simple-http)
(require html-parsing)
(require html-writing)
(require sxml/sxpath)
(require redis)

(define-syntax-rule (extract-from-tweet fn path tweet)
  (fn ((sxpath path) tweet)))

;; Define the default user and action
(define twitter-user (make-parameter "AbePalmer"))
(define action (make-parameter "display"))

(define (get-tweets name)
  """Get Tweets from a user's feed"""
  (let* [(requester (update-ssl (update-host json-requester "syndication.twitter.com") #t))
         (params `((dnt . "false")
                   (screen_name . ,name)
                   (suppress_response_codes . "true")
                   (lang . "en")
                   (with_replies . "false")
                   (hide_thread . "t")
                   (rnd . ,(number->string (random 10000000)))))
         (html-body (html->xexp
                     (hash-ref (json-response-body
                                (get requester "/timeline/profile" #:params params))
                               'body)))
         (query-tweets (sxpath "//li[contains(@class, 'timeline-Tweet')]/div[contains(@class, 'timeline-Tweet')]"))]
    (query-tweets html-body)))

(define (tweet->json tweet)
  (let [(author (extract-from-tweet car "//span[contains(@class, 'TweetAuthor-name')]/text()" tweet))
        (time-posted (extract-from-tweet cadar "//time/@title" tweet))
        (raw-html (extract-from-tweet xexp->html "//p[contains(@class, 'timeline-Tweet-text')]" tweet))]
    (string->bytes/utf-8 (string-append
                          "{'author': '"
                          author
                          "', 'time': '"
                          time-posted
                          "', 'tweet': '"
                          raw-html
                          "' }"))))

(define (display-tweets name)
  (for-each (lambda (tweet)
              (begin
                (newline)
                (display (tweet->json tweet))
                (newline)))
            (get-tweets name)))

(define (store-tweets name)
  """Store tweets to a list"
  (let [(c (make-redis))]
    (for-each (lambda (tweet)
                (redis-list-append!
                 c name
                 (tweet->json tweet)))
              (get-tweets name) )))

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
  [(equal? (action) "store") (store-tweets (twitter-user))]
  [(equal? (action) "display") (display-tweets (twitter-user))]
  [else (display "Please perform the correct action")])
