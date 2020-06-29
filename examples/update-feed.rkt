#! /usr/bin/env racket
#lang racket

#| Worker for fetching posts from twitter

Run the worker after every N hours vis-a-vis an infinite loop

This is a demo. Update as required!

|#

(require feedanalyser)

(define client (make-redis))


;;; Config params for the tweets
(define users "wolfgangkhuber,Y_Gliad,MarkGerstein,mstephens999,PaulFlicek,SagivShifman,Jericho,danjgaffney,bartdeplancke,robbie_stats,ClarissaCParker,DavidAshbrook,StatGenDan,GSCollins,MikeBradburn2,tobiaskurth,yudapearl,phuenermund")

(define search-terms "(genenetwork OR genenetwork2 OR rat OR mouse OR biology OR statistics) -Trump -trump")

(define (hours->seconds hours) (* hours 60 60))


;;; Config params for the repos to fetch from
(define repos '(("BonfaceKilz" . "feedanalyser")
                ("genenetwork" . "genenetwork2")
                ("arvados" . "bh20-seq-resource")))

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
   (get-tweets/twitter users
                       #:search-terms search-terms))


  ;; Adding commits
  (displayln "Adding commits:")

  (remove-expired-commits! client)

  (for-each
   (lambda (repo)
     (store-commits!
      client
      (get-commits/github (car repo)
                          (cdr repo))))
   repos)


  (sleep (hours->seconds 12))
  (loop))
