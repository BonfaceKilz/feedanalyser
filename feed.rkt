#lang racket/base

(require simple-http)
(require json)
(require html-parsing)
(require sxml/sxpath)
(require redis)

(define (get-tweets name)
  """Get Tweets from a user's feed"""
  (let* [(requester (update-ssl (update-host json-requester "syndication.twitter.com") #t))
         (params `((dnt . "false")
                   (screen_name . ,name)
                   (suppress_response_codes . "true")
                   (lang . "en")
                   (rnd . ,(number->string (random 10000000)))))
         (html-body (html->xexp
                     (hash-ref (json-response-body
                                (get requester "/timeline/profile" #:params params))
                               'body)))
         (query (sxpath "//p[contains(@class, 'timeline-Tweet-text')]/text()"))]
    (query html-body)))

(define (store-tweets tweets queue-name)
  """Store tweets to a list"
  (let [(c (make-redis))
        (for-each (lambda (xs fn)
                    (for ([x xs])
                      (fn x))))]
    (for-each tweets (lambda (tweet)
                       (redis-list-append!
                        c queue-name
                        (string->bytes/utf-8 tweet))))))

;;; Store the tweets
(store-tweets (get-tweets "BonfaceKilz")
              "BonfaceKilz")
