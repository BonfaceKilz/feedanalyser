#lang racket/base

(require simple-http)
(require json)
(require html-parsing)
(require sxml/sxpath)

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

;;; Demo: display tweets
;;; TODO: store later in some db
(display tweets)

