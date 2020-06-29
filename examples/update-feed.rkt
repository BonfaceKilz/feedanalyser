#! /usr/bin/env racket
#lang racket

#| Worker for fetching posts from twitter

Run the worker after every N hours vis-a-vis an infinite loop

This is a demo. Update as required!

|#

(require racket/cmdline
         feedanalyser)


;; Conf parameters
(define conf-filepath (make-parameter #f))
(define redis-conf (make-parameter '((host . "127.0.0.1")
                                     (port . 6379)
                                     (username . #f)
                                     (password . #f))))
(define refresh-time/hrs (make-parameter 24))

;;; Default repos to fetch data from
(define repos (make-parameter '(("genenetwork" . "genenetwork2")
                                ("arvados" . "bh20-seq-resource"))))

;;; Default params for twitter
(define search-terms
  (make-parameter "(genenetwork OR genenetwork2 OR rat OR mouse OR biology OR statistics) -Trump -trump"))
(define twitter-users
  (make-parameter "wolfgangkhuber,Y_Gliad,MarkGerstein,mstephens999,PaulFlicek,SagivShifman,Jericho,danjgaffney,bartdeplancke,robbie_stats,ClarissaCParker,DavidAshbrook,StatGenDan,GSCollins,MikeBradburn2,tobiaskurth,yudapearl,phuenermund"))


(define parser
  (command-line
   #:usage-help
   "Start the update script"
   #:once-each
   [("-c" "--conf") filepath "Configuration file path"
    (conf-filepath filepath)]
   #:args () (void)))

(cond
 [(conf-filepath)
  (let [(server/settings
         (load-config (conf-filepath)))]
    (redis-conf (hash-ref server/settings 'redis-conf))
    (refresh-time/hrs  (hash-ref server/settings 'refresh-time/hrs))
    (repos (hash-ref server/settings 'repos))
    (search-terms (hash-ref server/settings 'search-terms))
    (twitter-users (hash-ref server/settings 'twitter-users)))])


(define client (make-redis #:host (assoc-val 'host (redis-conf))
                           #:port (assoc-val 'port (redis-conf))
                           #:username (assoc-val 'username (redis-conf))
                           #:password (assoc-val 'password (redis-conf))))


(define (hours->seconds hours) (* hours 60 60))


(void
 (thread
  (lambda _
    (let loop ()
      (sleep 10)
      (loop)))))

(let loop ()
  ;; Adding tweets
  (displayln "Adding tweets:")
  (remove-expired-tweets! client)
  (store-tweets!
   client
   (get-tweets/twitter (twitter-users)
                       #:search-terms (search-terms)))
  (displayln "Done Adding tweets")


  ;; Adding commits
  (displayln "Adding commits:")
  (remove-expired-commits! client)
  (for-each
   (lambda (repo)
     (store-commits!
      client
      (get-commits/github (car repo)
                          (cdr repo))))
   (repos))
  (displayln "Done Adding commits")

  ;; Refresh after x hours
  (sleep (hours->seconds (refresh-time/hrs)))
  (loop))
