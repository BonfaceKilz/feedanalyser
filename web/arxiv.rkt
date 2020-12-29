#lang racket/base

(require threading
         racket/list
         racket/function
         racket/port
         racket/string
         redis
         simple-http
         sxml/sxpath
         "common.rkt"
         "votes.rkt")

(provide parse-arxiv-search-terms
         sxpath->feed-struct/arxiv
         store-arxiv-articles!
         vote-arxiv-article!
         get-articles/arxiv
         (struct-out feed-arxiv))

(struct feed-arxiv
  (title
   authors
   abstract
   submission
   url
   hash) #:transparent)

(define (sxpath->feed-struct/arxiv sxml)
  (define query (curry sxml-query sxml))
  (define (clean-input x replace-val/string)
    (~> x
        ((curryr string-replace replace-val/string ""))
        string-normalize-spaces))
  (~> sxml
      ((sxpath "//span[contains(@class, 'abstract-short')]/text()"))
      string-join
      ((curryr clean-input "")))
  (let* ([title (~> (query "//p[contains(@class, 'title')]")
                    ((curryr clean-input "")))]
         [authors (~> (query "//p[contains(@class, 'authors')]")
                      ((curryr clean-input "Authors:")))]
         [abstract (~> sxml
                       ((sxpath (string-append "//span[contains(@class,"
                                               " 'abstract-short')]/text()")))
                       string-join
                       ((curryr clean-input "")))]
         [submission (~> (query "//p[contains(@class, 'is-size-7')]")
                         ((curryr clean-input "Submitted"))
                         ((curryr string-split ";"))
                         car
                         string-normalize-spaces)]
         [url (~> sxml
                  ((sxpath (string-append "//p[contains(@class, "
                                          "'list-title')]/a/@href/text()")))
                  car)]
         [hash (~> title equal-hash-code number->string)])
    (apply feed-arxiv
           (list title
                 authors
                 abstract
                 submission
                 url
                 hash))))

(define (parse-arxiv-search-terms n counter terms)
  (define counter/num (number->string counter))
  (if (empty? terms)
      n
      (let* ([counter/num (number->string counter)]
             [operator (~> (string-append "terms-"
                                          counter/num
                                          "-operator")
                           string->symbol)]
             [term (~> (string-append "terms-"
                                      counter/num
                                      "-term")
                       string->symbol)]
             [field (~> (string-append "terms-"
                                       counter/num
                                       "-field")
                        string->symbol)]
             (op/value (~> terms caar symbol->string))
             (term/value (~> terms cadar symbol->string))
             (field/value (~> terms caddar symbol->string)))
        (parse-arxiv-search-terms (append n
                                          `((,operator . ,op/value)
                                            (,term . ,term/value)
                                            (,field . ,field/value)))
                                  (+ 1 counter)
                                  (rest terms)))))

(define (get-articles/arxiv search-terms)
  (let ([requester (update-ssl
                    (update-host html-requester
                                 "arxiv.org")
                    #t)]
        [params `(,@(parse-arxiv-search-terms '() 0 search-terms)
                  (advanced . "")
                  (classification-physics_archives . "all")
                  (classification-include_cross_list . "include")
                  (date-filter_by . "all_dates")
                  (date-date_type . "submitted_date")
                  (date-year . "")
                  (date-from_date . "")
                  (date-to_date . "")
                  (abstracts . "show")
                  (size . "25")
                  (order . "-announced_date_first")
                  (source . "home-covid-19"))])
    (~> (get requester "/search/advanced" #:params params)
        html-response-body
        ((curryr map-xexp
                (string-append "//li[contains(@class, "
                               "'arxiv-result')]")
                sxpath->feed-struct/arxiv)))))

(define (store-arxiv-articles! client
                               articles
                               #:feed-prefix [feed-prefix ""])
  (define (store-article! c article)
    (let* ([title (feed-arxiv-title article)]
           [authors (feed-arxiv-authors article)]
           [abstract (feed-arxiv-abstract article)]
           [submission (feed-arxiv-submission article)]
           [url (feed-arxiv-url article)]
           [hash (feed-arxiv-hash article)]
           [key (string-append
                 feed-prefix
                 "arxiv:"
                 (if (string? hash)
                     hash
                     (bytes->string/utf-8 hash)))])
      (unless (redis-has-key? c key)
        (redis-zset-add! c
                         (string-append feed-prefix "arxiv-score:")
                         key
                         0)
        (redis-hash-set! c key "title" title)
        (redis-hash-set! c key "authors" authors)
        (redis-hash-set! c key "abstract" abstract)
        (redis-hash-set! c key "submission" submission)
        (redis-hash-set! c key "url" url)
        (redis-hash-set! c key "hash" key)
        (redis-hash-set! c key "score" "0")
        ;; Expire after 30 days
        (redis-expire-in! c key (* 30 24 60 60 100)))))
  (~>> articles
       (map (curry serialize-struct feed-arxiv))
       (map (curry store-article! client))))

(define (vote-arxiv-article! client key
                             #:upvote? [upvote? #t]
                             #:feed-prefix [feed-prefix ""])
  ;; Increase expiry by 30 days
  (redis-expire-in! client key (* 30 24 60 60 100))
  (vote! client
         (string-append feed-prefix "arxiv-score:")
         key
         #:upvote? upvote?))
