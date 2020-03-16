#lang racket/base

(require simple-http)
(require json)
(require html-parsing)
(require html-writing)
(require sxml/sxpath)
(require redis)

(define (get-tweets name)
  """Get Tweets from a user's feed"""
  (let [(requester (update-ssl (update-host json-requester "syndication.twitter.com") #t))
        (params `((dnt . "false")
                  (screen_name . ,name)
                  (suppress_response_codes . "true")
                  (lang . "en")
                  (rnd . ,(number->string (random 10000000)))))]
    (get
     requester "/timeline/profile" #:params params)))

(define tweets
  (let [(html-body (html->xexp
                    (hash-ref (json-response-body (get-tweets "AbePalmer"))
                              'body)))
        (query (sxpath '(// (p (@ (equal? (class "timeline-Tweet-text")))))))]
    (query html-body)))

(define (store-tweets tweets queue-name)
  """Store tweets to a list"
  (let [(c (make-redis))
        (for-each (lambda (xs fn)
                    (for ([x xs])
                      (fn x))))]
    (for-each tweets (lambda (tweet)
                         (redis-list-prepend! c queue-name (xexp->html-bytes tweet))))))

;;; Store the tweets
(store-tweets tweets "AbePalmer");
