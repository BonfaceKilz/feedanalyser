#lang racket

(provide ghcommits->hash
         store-gh-commits)

(require simple-http)
(require json)
(require redis)
(require lens/common)
(require lens/data/hash)

(define (ghcommits->hash repo-name)
  """Get commits from a repo"""
  (let* [(requester (update-ssl (update-host json-requester "api.github.com") #t))
         (params '((page . "1") (per_page . "10")))]
    (json-response-body
     (get requester (string-append "/repos/graph-genome/" repo-name "/commits") #:params params))))

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

(define (read-gh-commits repository)
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
