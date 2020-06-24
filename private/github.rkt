#lang racket

(require json
         lens/common
         lens/data/hash
         redis
         simple-http)


(provide ghcommits->hash
         store-gh-commits
         read-gh-commits)


(define (ghcommits->hash repo-name)
  """Get commits from a repo"""
  (let* ([requester (update-ssl (update-host json-requester "api.github.com") #t)]
         [params '((page . "1") (per_page . "10"))])
    (json-response-body
     (get requester (string-append "/repos/graph-genome/" repo-name "/commits") #:params params))))


(define (store-gh-commits client repository)
  """Store commits to a list"
  (let ([commit-lens (lens-compose (hash-pick-lens 'author 'message 'url)
                                   (hash-ref-lens 'commit))])
    (redis-remove! client "Github")
    (for-each (lambda (commit)
                (redis-list-append!
                 client
                 "Github"
                 (jsexpr->bytes (lens-view commit-lens commit))))
              (ghcommits->hash repository))))


(define (read-gh-commits client repository)
  """Store commits to a list"
  (let ([commit-lens (lens-compose (hash-pick-lens 'author 'message 'url)
                                   (hash-ref-lens 'commit))])
    (for-each (lambda (commit)
                (redis-list-append!
                 client
                 "Github"
                 (jsexpr->bytes (lens-view commit-lens commit))))
              (ghcommits->hash repository))))
