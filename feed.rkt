#! /usr/bin/env racket
#lang racket

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

(define (ghcommits->hash repo-name)
  """Get commits from a repo"""
  (let* [(requester (update-ssl (update-host json-requester "api.github.com") #t))
         (params `'((page . "1") (per_page . "10")))]
    (json-response-body
     (get requester "/repos/graph-genome/Schematize/commits" #:params params))))

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


(define (store-gh-commits repository)
  """Store commits to a list"
  (let [(c (make-redis))
        (commit-lens (lens-compose (hash-pick-lens 'author 'message 'url)
                                   (hash-ref-lens 'commit)))]
    (for-each (lambda (commit)
                (redis-list-append!
                 c
                 "Github"
                 (jsexpr->bytes (lens-view commit-lens commit))))
              (ghcommits->hash repository))))

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
