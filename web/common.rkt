#lang racket/base

(require racket/string
         racket/struct
         racket/function
         redis
         sxml
         sxml/sxpath
         threading
         "votes.rkt")

(provide remove-markup
         remove-expired-items!
         remove-all-items!
         serialize-struct
         sxml-query
         map-xexp
         get-items/redis)

;; Adapted from:
;; https://docs.racket-lang.org/sxml/ssax.html?q=srl%3Asxml-%3Ehtml
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

(define (serialize-struct struct-fn feed/struct)
  "serialize a struct FEED/STRUCT using the constructor STRUCT-FN"
  (apply struct-fn
         (~>> (~> feed/struct
                  struct->list)
              (map (lambda (x)
                     (cond ((number? x)
                            (~> x
                             number->string
                             string->bytes/utf-8))
                           ((string? x)
                            (~> x
                                string->bytes/utf-8))
              (else x)))))))

(define (sxml-query sxml query/string)
  "sxml query to extract elements given: sxml content SXML, a html
element EL and a class CLASS-STRING"
    (~> sxml
        ((sxpath query/string))
        srl:sxml->html
        open-input-string
        remove-markup
        string-normalize-spaces))

(define (map-xexp xexp query fn)
  "Apply FN to list of XEXPs extracted using QUERY"
  (~>> (~> xexp
           ((sxpath query)))
       (map fn)))

(define (get-items/redis client
                         #:key [key ""]
                         #:start [start 0]
                         #:stop [stop -1]
                         #:reverse? [reverse? #t]
                         #:feed-prefix [feed-prefix ""])
  (~>> (redis-subzset client
                      (string-append feed-prefix key)
                      #:start start
                      #:stop stop
                      #:reverse? reverse?)
       (map (curry redis-hash-get client))))

(define (remove-expired-items! client
                               zset/list
                               #:feed-prefix [feed-prefix ""])
  "Remove all expired elements from the zset ZSET/LIST"
  (remove-expired-keys! client
                        (~>> zset/list
                             (map (curry string-append feed-prefix)))))

(define (remove-all-items! client key-regex #:feed-prefix [feed-prefix ""])
  "Remove all items from Redis that match KEY-REGEX .e.g. 'tweet*'"
  (remove-all-keys! client
                    (string-append feed-prefix key-regex)))
