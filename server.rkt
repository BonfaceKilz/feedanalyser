#lang racket

(require web-server/servlet-env)
(require web-server/templates)
(require web-server/http/response-structs)

(require SSE)

;; Page that connects to the server-sent events
(define (start-page req)
  (response/output
   (λ (op) (display (include-template "templates/feed.html") op))))

;; starts the page servlet
(serve/servlet start-page
               #:launch-browser? #f
               #:quit? #f
               #:servlet-path "/"
               #:port 8080)
