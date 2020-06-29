#! /usr/bin/env racket
#lang racket


(require racket/cmdline
         feedanalyser)


;; Conf parameters
(define conf-filepath (make-parameter #f))
(define redis-conf (make-parameter '((host . "127.0.0.1")
                                     (port . 6379)
                                     (username . #f)
                                     (password . #f))))
(define server-port (make-parameter 6379))
(define log-file/path (make-parameter "feed.log"))


(define parser
  (command-line
   #:usage-help
   "Start the polling server"
   #:once-each
   [("-c" "--conf") filepath "Configuration file path"
    (conf-filepath filepath)]

   #:args () (void)))


(define (assoc-val key assoc-list)
  (cdr (assoc key assoc-list)))
(cond
 [(conf-filepath)
  (let [(server/settings
         (load-config (conf-filepath)))]
    (redis-conf (hash-ref server/settings 'redis-conf))
    (server-port (hash-ref server/settings 'server-port))
    (log-file/path (hash-ref server/settings 'log-file)))])


(define client (make-redis #:host (assoc-val 'host (redis-conf))
                           #:port (assoc-val 'port (redis-conf))
                           #:username (assoc-val 'username (redis-conf))
                           #:password (assoc-val 'password (redis-conf))))

(start-server client #:port (server-port) #:log-file (log-file/path) )
