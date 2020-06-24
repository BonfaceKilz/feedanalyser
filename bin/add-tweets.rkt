#lang racket/base

#| Worker for fetching posts from twitter

Run the worker after every N hours vis-a-vis an infinite loop

This is a demo. Update as required!

|#

(require feedanalyser)

(define client (make-redis))

(define users "wolfgangkhuber,Y_Gliad,MarkGerstein,mstephens999,PaulFlicek,SagivShifman,Jericho,danjgaffney,bartdeplancke,robbie_stats,ClarissaCParker,DavidAshbrook,StatGenDan,GSCollins,MikeBradburn2,tobiaskurth,yudapearl,phuenermund")

(define search-terms "genenetwork OR genenetwork2 OR rat OR science")

(define search-query
  "genenetwork OR genenetwork2 OR rat OR science")

(define (hours->seconds hours) (* hours 60 60))

(void
 (thread
  (lambda _
    (let loop ()
      (sleep 10)
      (loop)))))

(let loop ()
  (displayln "Adding tweets:")
  (remove-expired-tweets! client)
  (store-tweets!
   client
   (get-tweets/twitter users
                       #:search-terms search-terms))
  (sleep (hours->seconds 12))
  (loop))
