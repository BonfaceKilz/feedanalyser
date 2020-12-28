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
         "votes.rkt")

(provide get-articles/pubmed
         get-pubmed-articles/redis
         xexp->list/pubmed-feed-struct
         serialize-pubmed-feed
         store-pubmed-articles!
         remove-expired-articles!
         remove-all-articles!
         vote-article!
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


(define (sxpath->feed-struct sxml)
  "Extract feed-struct out of a given pubmed sxpath"
  (define (remove-markup xml-port)
    "Given a string with markup, remove the markup"
    (define (remove-markup-nls gi attributes namespaces expected-content
                               seed)
      seed)

    (define (remove-markup-fe gi attributes namespaces parent-seed seed)
      seed)

    (define (remove-markup-cdh string-1 string-2 seed)
      (let ((seed (cons string-1 seed)))
        (if (non-empty-string? string-2)
            (cons string-2 seed)
            seed)))

    (let* ((parser
            (ssax:make-parser NEW-LEVEL-SEED remove-markup-nls
                              FINISH-ELEMENT remove-markup-fe
                              CHAR-DATA-HANDLER remove-markup-cdh))
           (strings (parser xml-port null)))
      (string-join (reverse strings) "")))

  (define (query el class-string)
    (~> sxml
        ((sxpath
         `(// (,el (@ (equal? (class ,class-string)))))))
        srl:sxml->html
        open-input-string
        remove-markup
        string-normalize-spaces))

  (let ([full-authors (query 'span "docsum-authors full-authors")]
        [short-authors (query 'span "docsum-authors short-authors")]
        [citation (query 'span "docsum-journal-citation full-journal-citation")]
        [short-journal-citation (query 'span "docsum-journal-citation short-journal-citation")]
        [docsum-pmid (query 'span "docsum-pmid")]
        [summary (query 'a "docsum-title")])
    (feed-pubmed
     full-authors
     short-authors
     citation
     short-journal-citation
     summary
     docsum-pmid)))

(define (xexp->list/pubmed-feed-struct html)
  "Extract html from input and return dict of values"
  (let ([articles/xml
         (~> html
             ((sxpath '(// (div (@ (equal? (class "docsum-content"))))))))])
    (~>> articles/xml
         (map sxpath->feed-struct))))

(define (get-articles/pubmed search-terms/string)
  (let ([requester (update-ssl
                    (update-host html-requester
                                 "pubmed.ncbi.nlm.nih.gov") #t)]
        [params `((term . ,(~> search-terms/string
                               form-urlencoded-encode))
                  (show_snippets . "off")
                  (sort . "pubdate")
                  (sort_order . "desc")
                  (size . "20"))])
    (~> (get requester "/" #:params params)
        html-response-body
        xexp->list/pubmed-feed-struct)))

(define (serialize-pubmed-feed article)
  (apply feed-pubmed (~>> (~> article struct->list)
                         (map string->bytes/utf-8))))

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
       (map serialize-pubmed-feed)
       (map (curry store-article! client))))

(define (get-pubmed-articles/redis
         client
         #:key [key "pubmed-score:"]
         #:start [start 0]
         #:stop [stop -1]
         #:reverse? [reverse? #t]
         #:feed-prefix [feed-prefix ""])
  (~>> (redis-subzset
        client
        (string-append feed-prefix key)
        #:start start
        #:stop stop
        #:reverse? reverse?)
       (map (curry redis-hash-get client))))


(define (remove-expired-articles! client #:feed-prefix [feed-prefix ""])
  (remove-expired-keys! client (list
                                (string-append feed-prefix "pubmed-score:"))))


(define (remove-all-articles! client #:feed-prefix [feed-prefix ""])
  (remove-all-keys! client
                    (string-append feed-prefix "pubmed*")))

(define (vote-article! client key
                       #:upvote? [upvote? #t]
                       #:feed-prefix [feed-prefix ""])
  ;; Increase expiry by 30 days
  (redis-expire-in! client key (* 30 24 60 60 100))
  (vote! client (string-append feed-prefix "pubmed-score:")
         key #:upvote? upvote?))
