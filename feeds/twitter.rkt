#lang racket

(provide get-tweets
         tweet->json
         display-tweets
         store-tweets
         has-words?
         list->regex-string)

(require simple-http)
(require html-parsing)
(require html-writing)
(require sxml)
(require xml)
(require sxml/sxpath)
(require json)
(require redis)
(require lens/common)
(require lens/data/hash)

(define-syntax-rule (extract-from-tweet fn path tweet)
  (fn ((sxpath path) tweet)))

(define (list->regex-string xs)
  """Given a list of strings, generate a valid regex string separated by '|' """
  (define (iter x regex-string)
    (cond [(null? x) ""]
          [(<= (length x) 1)
           (string-append regex-string "|" (car x))]
          [else
           (if (string=? regex-string "")
               (iter (cdr x) (string-append regex-string (car x)))
               (iter (cdr x) (string-append regex-string "|" (car x))))]))
  (iter xs ""))

(define (has-words? xs)
  (lambda (x)
    (regexp-match
     (list->regex-string xs)
     (extract-from-tweet
      car
      "//text()"
      (html->xexp
       (hash-ref
        (string->jsexpr (tweet->json x))
        'tweet))))))

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
  "Convert a single tweet to a valid JSON object"
  (let [(author (extract-from-tweet car "//span[contains(@class, 'TweetAuthor-screenName')]/text()" tweet))
        (time-posted (extract-from-tweet cadar "//time/@title" tweet))
        (raw-html (srl:sxml->html (extract-from-tweet car "//p[contains(@class, 'timeline-Tweet-text')]" tweet)))]
    (jsexpr->string (make-hash `((author . ,author)
                                 (time-posted . ,time-posted)
                                 (tweet . ,raw-html))))))

(define (display-tweets name filter-fn)
  "Display tweets to STDOUT"
  (for-each (lambda (tweet)
              (begin
                (newline)
                (newline)
                (display (tweet->json tweet))
                (newline)))
            (filter filter-fn
                    (get-tweets name))))

(define (store-tweets name filter-fn)
  """Store tweets to a list"
  (let [(c (make-redis))]
    (for-each (lambda (tweet)
                (redis-list-append!
                 c
                 name
                 (string->bytes/utf-8 (tweet->json tweet))))
              (filter filter-fn
                      (get-tweets name)))))
