#lang racket/base

(require threading
         racket/function
         racket/string
         racket/struct
         redis
         simple-http
         sxml
         net/uri-codec
         sxml/sxpath
         "common.rkt"
         "votes.rkt")

(provide get-articles/pubmed
         store-pubmed-articles!
         remove-expired-articles!
         remove-all-articles!
         vote-pubmed-article!
         sxpath->feed-struct/pubmed
         (struct-out feed-pubmed))


; A simple (placeholder) pubmed type, that contains metadata about
; articles
(struct feed-pubmed
  (full-authors
   short-authors
   citation
   short-journal-citation
   summary
   docsum-pmid) #:transparent)


(define (sxpath->feed-struct/pubmed sxml)
  "Extract feed-struct out of a given pubmed sxpath"
  (define query (curry sxml-query sxml))
  (let ([full-authors (query "//span[contains(@class, 'full-authors')]")]
        [short-authors (query "//span[contains(@class, 'short-authors')]")]
        [citation (query "//span[contains(@class, 'full-journal-citation')]")]
        [short-journal-citation (query "//span[contains(@class, 'short-journal-citation')]")]
        [docsum-pmid (query "//span[contains(@class, 'docsum-pmid')]")]
        [summary (query "//a[contains(@class, 'docsum-title')]")])
    (apply feed-pubmed
           (list full-authors
                 short-authors
                 citation
                 short-journal-citation
                 summary
                 docsum-pmid))))


(define (get-articles/pubmed search-terms/string)
  (let ([requester (update-ssl
                    (update-host html-requester
                                 "pubmed.ncbi.nlm.nih.gov") #t)]
        [params `((term . ,(~> search-terms/string
                               form-urlencoded-encode))
                  (show_snippets . "off")
                  (sort . "pubdate")
                  (sort_order . "desc")
                  (filter . "simsearch2.ffrft")
                  (filter . "articleattr.data")
                  (size . "20"))])
    (~> (get requester "/" #:params params)
        html-response-body
        ((curryr map-xexp
                (string-append "//div[contains(@class, "
                               "'docsum-content')]")
                sxpath->feed-struct/pubmed)))))


(define (store-pubmed-articles! client articles
                               #:feed-prefix [feed-prefix ""])
  (define (store-article! c article)
    (let* ([full-authors (feed-pubmed-full-authors article)]
           [short-authors (feed-pubmed-short-authors article)]
           [citation (feed-pubmed-citation article)]
           [short-journal-citation
            (feed-pubmed-short-journal-citation article)]
           [summary (feed-pubmed-summary article)]
           [docsum-pmid (feed-pubmed-docsum-pmid article)]
           [key (string-append
                 feed-prefix
                 "pubmed:"
                 (if (string? docsum-pmid)
                     docsum-pmid
                     (bytes->string/utf-8 docsum-pmid)))])
      (unless (redis-has-key? c key)
        (redis-zset-add!
         c
         (string-append feed-prefix "pubmed-score:")
         key
         0)
        (redis-hash-set! c key "full-authors" full-authors)
        (redis-hash-set! c key "short-authors" short-authors)
        (redis-hash-set! c key "citation" citation)
        (redis-hash-set!
         c key "short-journal-citation" short-journal-citation)
        (redis-hash-set! c key "summary" summary)
        (redis-hash-set! c key "score" "0")
        (redis-hash-set! c key "docsum-pmid" docsum-pmid)
        ;; Expire after 30 days
        (redis-expire-in! c key (* 30 24 60 60 100)))))
  (~>> articles
       (map (curry serialize-struct feed-pubmed))
       (map (curry store-article! client))))


(define (remove-expired-articles! client #:feed-prefix [feed-prefix ""])
  (remove-expired-keys! client (list
                                (string-append feed-prefix "pubmed-score:"))))


(define (remove-all-articles! client #:feed-prefix [feed-prefix ""])
  (remove-all-keys! client
                    (string-append feed-prefix "pubmed*")))

(define (vote-pubmed-article! client key
                       #:upvote? [upvote? #t]
                       #:feed-prefix [feed-prefix ""])
  ;; Increase expiry by 30 days
  (redis-expire-in! client key (* 30 24 60 60 100))
  (vote! client (string-append feed-prefix "pubmed-score:")
         key #:upvote? upvote?))
