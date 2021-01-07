#! /usr/bin/env racket
#lang racket

#| Worker for fetching posts from twitter

Run the worker after every N hours vis-a-vis an infinite loop

This is a demo. Update as required!

|#

(require racket/cmdline
         feedanalyser)


(define conf-filepath (make-parameter #f))

(define parser
  (command-line
   #:usage-help
   "Start the update script"
   #:once-each
   [("-c" "--conf") filepath "Configuration file path"
    (conf-filepath filepath)]
   #:args () (void)))


(unless (conf-filepath)
  (error "Please provide a configuration file!"))


(define server/settings (load-config (conf-filepath)))


;; Conf parameters
(define refresh-time/hrs (hash-ref server/settings 'refresh-time/hrs))


;; Feed prefix for each feed
(define feed-prefix (hash-ref server/settings 'feed-prefix))


;;; Params for commits feed
(define repos (hash-ref server/settings 'repos))


;;; Params for twitter feed
(define tweets-per-user (hash-ref server/settings
                                  'tweets-per-user))
(define min-retweets (hash-ref server/settings
                               'min-retweets))
(define twitter-users (hash-ref server/settings
                                'twitter-users))
(define twitter-search-terms (hash-ref server/settings
                                        'twitter-search-terms))


;;; Params for pubmed feed
(define pubmed-search-terms (hash-ref server/settings
                                      'pubmed-search-terms))


;;; Params for arxiv feed
(define arxiv-search-terms (hash-ref server/settings
                                     'arxiv-search-terms))


;; Redis Client
(define redis-conf (hash-ref server/settings 'redis-conf))
(define client (make-redis #:host (assoc-val 'host redis-conf)
                           #:port (assoc-val 'port redis-conf)
                           #:username (assoc-val 'username redis-conf)
                           #:password (assoc-val 'password redis-conf)))


(define (hours->seconds hours) (* hours 60 60))


(define script/refresh-contents-seconds
  (+ (current-seconds)
     (hours->seconds refresh-time/hrs)))


(define (expire-feed-items)
  (remove-expired-items! client
                         '("tweet-score:" "tweet-time:")
                         #:feed-prefix feed-prefix)
  (remove-expired-items! client
                         '("commit-score:" "commit-time:")
                         #:feed-prefix feed-prefix)
  (remove-expired-items! client
                         '("pubmed-score:")
                         #:feed-prefix feed-prefix)
  (remove-expired-items! client
                         '("arxiv-score:")
                         #:feed-prefix feed-prefix))


(define (add-content-to-redis)
  (displayln "Adding tweets:")
  (store-tweets!
   client
   (get-tweets/twitter twitter-users
                       #:search-terms twitter-search-terms
                       #:min-retweets min-retweets
                       #:number tweets-per-user)
   #:feed-prefix feed-prefix)
  (displayln "Done Adding tweets")
  ;; Adding commits
  (displayln "Adding commits:")
  (for-each
   (lambda (repo)
     (store-commits! client
                     (get-commits/github (car repo)
                                         (cdr repo))
                     #:feed-prefix feed-prefix))
   repos)
  (displayln "Done Adding commits")

  ;; Adding Pubmed Articles
  (displayln "Adding pubmed articles:")
  (store-pubmed-articles! client
                          (get-articles/pubmed pubmed-search-terms)
                          #:feed-prefix feed-prefix)
  (displayln "Done Adding articles")

  ;; Adding arxiv articles
  (displayln "Adding arxiv articles:")
  (store-arxiv-articles! client
                         (get-articles/arxiv arxiv-search-terms)
                         #:feed-prefix feed-prefix)
  (displayln "Done Adding arxiv artiles")
  
  (expire-feed-items))


;; Initial addition of contents
(add-content-to-redis)


(void
 (thread (lambda _
           (let loop ()
             (sleep 10)
             (loop)))))


(let loop ()
  (expire-feed-items)
  (when (> (current-seconds) script/refresh-contents-seconds)
    (set! script/refresh-contents-seconds
      (+ (current-seconds) (hours->seconds refresh-time/hrs)))
    (add-content-to-redis))
  (sleep 10)
  (loop))
