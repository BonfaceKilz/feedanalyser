#lang racket

(require json
         lens/common
         lens/data/hash
         redis
         simple-http)


(provide get-commits/github
         store-gh-commits
         read-gh-commits
         (struct-out feed-commit))


; A struct type to store details about commits from various places
(struct feed-commit (author content timeposted hash url) #:transparent)


;; Get tweets from github, storing them in a struct
(define (get-commits/github username reponame #:page [page 1] #:per-page [per-page 10])
  (define (->struct commit)
    (let* [(commit-dict (hash-ref commit 'commit))
           (author-dict (hash-ref commit-dict 'committer))
           (author (hash-ref author-dict 'name))
           (content (hash-ref commit-dict 'message))
           (timeposted (hash-ref author-dict 'date))
           (url (hash-ref commit 'html_url))
           (hash (hash-ref commit 'sha))
           (hash (hash-ref commit 'sha))]
      (feed-commit author content timeposted hash url)))

  (let* ([requester (update-ssl (update-host json-requester "api.github.com") #t)]
         [params `((page . ,(number->string page)) (per_page . ,(number->string per-page)))]
         [commits (json-response-body
                   (get requester
                        (string-append
                         "/repos/"
                         username
                         "/"
                         reponame
                         "/commits")
                        #:params params))])
    (map
     (lambda (commit)
       (->struct commit))
     commits)))


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
