#lang racket/base

(require threading
         racket/list
         racket/function
         racket/port
         racket/string
         simple-http
         sxml/sxpath
         "common.rkt"
         "votes.rkt")

(provide parse-arxiv-search-terms
         sxpath->feed-struct/arxiv
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
         [hash (~> title equal-hash-code)])
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
                                 "arxiv.org") #t)]
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
