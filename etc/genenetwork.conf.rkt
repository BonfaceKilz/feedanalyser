;;;; Genenetwork2 configuration file

(redis-conf . '((host . "127.0.0.1")
                (port . 6379)
                (username . #f)
                (password . #f)))

;; Prefix to the redis columns
(feed-prefix . "gn2-")

(tweets-per-user . 2)

;; The log file
(log-file . "voting-server.log")

;; The port to run the voting.html page from
(server-port . 6011)

;; Use any twitter's advanced search options here
(twitter-search-terms . "(genenetwork OR genenetwork2 OR rat OR mouse OR biology OR statistics) -Trump -trump")

(pubmed-search-terms . "(genenetwork OR genenetwork2 OR rat OR mouse OR biology OR statistics)")

;; Have a list of users from which you can fetch tweets from
(twitter-users . "wolfgangkhuber,Y_Gliad,MarkGerstein,mstephens999,PaulFlicek,SagivShifman,Jericho,danjgaffney,bartdeplancke,robbie_stats,ClarissaCParker,DavidAshbrook,StatGenDan,GSCollins,MikeBradburn2,tobiaskurth,yudapearl,phuenermund,pjotrprins,GeneNetwork2")

;; All the repos you want to fetch commits from
(repos . '(("genenetwork" . "genenetwork2")))

;; In the update-feed script example, sleep for 'X' amount of hours
;; after which resume updating
(refresh-time/hrs . 24)
