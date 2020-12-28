#lang racket/base

(require racket/string
         racket/struct
         sxml
         sxml/sxpath
         threading)

(provide remove-markup
         serialize-struct
         sxml-query)

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
  (apply struct-fn
         (~>> (~> feed/struct
                  struct->list)
              (map string->bytes/utf-8))))

(define (sxml-query sxml query/string)
  "sxml query to extract elements given: sxml content SXML, a html
element EL and a class CLASS-STRING"
    (~> sxml
        ((sxpath query/string))
        srl:sxml->html
        open-input-string
        remove-markup
        string-normalize-spaces))
