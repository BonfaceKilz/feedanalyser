;; PubSeq website conf file
(redis-conf . '((host . "127.0.0.1")
                (port . 6379)
                (username . #f)
                (password . #f)))

;; Prefix to the redis columns
(feed-prefix . "bh20-")

(tweets-per-user . 2)

;; The log file
(log-file . "bh20-voting-server.log")

;; The port to run the voting.html page from
(server-port . 8022)

;; Use any twitter's advanced search options here
(search-terms . "(covid OR covid19 OR corona) -Trump -trump")

(twitter-users . "BonfaceKilz,wolfgangkhuber,Y_Gliad,MarkGerstein,mstephens999,PaulFlicek,SagivShifman,Jericho,danjgaffney,bartdeplancke,robbie_stats,ClarissaCParker,DavidAshbrook,StatGenDan,GSCollins,MikeBradburn2,tobiaskurth,yudapearl,phuenermund,pjotrprins,GeneNetwork2")

;; All the repos you want to fetch commits from
(repos . '(("arvados" . "bh20-seq-resource")))

;; In the update-feed script example, sleep for 'X' amount of hours
;; after which resume updating
(refresh-time/hrs . 24)
