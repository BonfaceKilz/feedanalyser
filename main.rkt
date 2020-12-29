#lang racket

(require racket/exn)

(define-syntax-rule (reprovide e0 e ...)
  (begin
    (require e0 e ...)
    (provide (all-from-out e0 e ...))))

(reprovide
 "web/twitter.rkt"
 "web/github.rkt"
 "web/pubmed.rkt"
 "web/server.rkt"
 "web/common.rkt"
 "web/arxiv.rkt"
 redis)


(provide load-config
         assoc-val)


(define (assoc-val key assoc-list)
  (cdr (assoc key assoc-list)))

;; Example (load-config "/tmp/conf.rkt")
(define (load-config config-path)
  (with-handlers ([exn:fail:filesystem:errno?
                   (lambda (exn)
                     (error (exn->string exn)))])
    (let [(eval-ns (make-base-namespace))
          (params (make-hash))]
      (let [(settings (file->list config-path))]
        (for [(name:value settings)]
          (let* [(n (car name:value)); evaluate the 'value' expression
                 (v (cdr name:value))
                 (v (eval (quasiquote (,@v)) eval-ns))]
            (hash-set! params n v)
            )))
      params)))
