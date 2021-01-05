;;;; Genenetwork2 configuration file

(redis-conf . '((host . "127.0.0.1")
                (port . 6379)
                (username . #f)
                (password . #f)))

;; Prefix to the redis columns
(feed-prefix . "gn2-")

(tweets-per-user . 2)

;; The log file
(log-file . "gn2-voting-server.log")

;; The port to run the voting.html page from
(server-port . 8023)

;; Use any twitter's advanced search options here
(twitter-search-terms . "(genenetwork OR genenetwork2 OR rat OR mouse OR pangenome OR browser OR workflows OR bioinformatic OR GeneNetwork OR conference OR sequences OR genotype OR phenotype OR guix OR RDF OR SPARQL OR genome OR biology OR Genomics OR OPAR OR genetics OR research OR ontologies OR variation)")

(pubmed-search-terms . "(genenetwork[Title/Abstract] OR genenetwork2[Title/Abstract] OR rat[Title/Abstract] OR mouse[Title/Abstract] OR pangenome[Title/Abstract] OR genome browser[Title/Abstract] NOT (keyboard OR interactive))")

(arxiv-search-terms . '((AND genenetwork title)
                        (OR genenetwork2 abstract)
                        (OR genome-browser abstract)
                        (OR pangenome abstract)
                        (OR mouse title)
                        (NOT cursor title)
                        (NOT cursor abstract)
                        (NOT interactive title)
                        (NOT interactive abstract)
                        (NOT pointer title)
                        (NOT pointer abstract)
                        (NOT Computer+Mouse title)
                        (NOT Computer+Mouse abstract)
                        (NOT Mouse-tracking+data abstract)
                        (NOT keyboard title)
                        (NOT keyboard abstract)
                        (NOT interactive title)
                        (NOT interactive abstract)))

(min-retweets . 4)

;; Have a list of users from which you can fetch tweets from
(twitter-users . "wolfgangkhuber,Y_Gliad,MarkGerstein,mstephens999,PaulFlicek,SagivShifman,Jericho,danjgaffney,bartdeplancke,robbie_stats,ClarissaCParker,DavidAshbrook,StatGenDan,GSCollins,MikeBradburn2,tobiaskurth,yudapearl,phuenermund,pjotrprins,GeneNetwork2,commonwl,simonheumos,gedankenstuecke,DCGenomics,pvanheus,miles_benton,wait_sasha,ewanbirney,BenedictPaten,sjackman,webyrd,BioMickWatson,drtkeane,GA4GH,KaczorowskiLab,AMelquiond,justsaysinmice,OPARProject,erikgarrisonDannyArends,JamesEKrause,Eric_Fauman,jlane_boston,arcova,FrontGenetics,AngelicaLeeOli1,bonfacekilz,fermatslibrary,arxiv,francois_sabot,metegenomez,MColebrook,GenoLabgem,pangenomepapers,firefoxx66")


;; All the repos you want to fetch commits from
(repos . '(("genenetwork" . "genenetwork2")))

;; In the update-feed script example, sleep for 'X' amount of hours
;; after which resume updating
(refresh-time/hrs . 24)
