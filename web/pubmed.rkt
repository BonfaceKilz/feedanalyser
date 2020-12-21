#lang racket/base

(require threading
         racket/string
         racket/struct
         sxml
         html-parsing
         sxml/sxpath)

(provide extract-content/articles
         serialize-pubmed-feed
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

(define (extract-content/articles html)
  "Extract html from input and return dict of values"
  (let ([articles/xml
         (~> html
             html->xexp
             ((sxpath '(// (div (@ (equal? (class "docsum-content"))))))))])
    (~>> articles/xml
         (map sxpath->feed-struct))))

(define (serialize-pubmed-feed article)
  (apply feed-pubmed (~>> (~> article struct->list)
                         (map string->bytes/utf-8))))
