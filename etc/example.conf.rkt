;;;; Example configuration file

(redis-conf . '((host . "127.0.0.1")
                (port . 6379)
                (username . #f)
                (password . #f)))

;; The log file
(log-file . "polling-server.log")

;; The port to run the polling html page from
(server-port . 5011)

;; Use any twitter's advanced search options here
(search-terms . "(genenetwork OR genenetwork2 OR rat OR mouse OR biology OR statistics) -Trump -trump")


;; Have a list of users from which you can fetch tweets from
(twitter-users . "wolfgangkhuber,Y_Gliad,MarkGerstein,mstephens999,PaulFlicek,SagivShifman,Jericho,danjgaffney,bartdeplancke,robbie_stats,ClarissaCParker,DavidAshbrook,StatGenDan,GSCollins,MikeBradburn2,tobiaskurth,yudapearl,phuenermund")

;; All the repos you want to fetch commits from
(repos . '(("BonfaceKilz" . "feedanalyser")
           ("genenetwork" . "genenetwork2")
           ("arvados" . "bh20-seq-resource")))

;; In the update-feed script example, sleep for 'X' amount of hours
;; after which resume updating
(refresh-time/hrs . 24)
