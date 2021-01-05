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
(twitter-search-terms . "(covid OR covid19 OR corona OR COVID OR COVID19 OR SARS2 OR SARS-CoV-2 OR SARS OR CoV2) -Trump -trump")

;; This differst slightly with twitter's advanced search
(pubmed-search-terms . "covid[Title/Abstract] OR corona virus[Title/Abstract] OR covid19[Title/Abstract] OR sars2[Title/Abstract] OR SARS-CoV-2[Title/Abstract] OR SARS-CoV2")

(arxiv-search-terms . '((AND COVID-19 title)
                        (OR SARS-CoV-2 abstract)
                        (OR COVID-19 abstract)
                        (OR SARS-CoV-2 title)
                        (OR coronavirus title)
                        (OR coronavirus abstract)))

(min-retweets . 20)

;; Limit from twitter is 20 users
;; See: https://github.com/twintproject/twint/issues/360#issuecomment-464308253
(twitter-users . "BonfaceKilz,wolfgangkhuber,MarkGerstein,mstephens999,PaulFlicek,SagivShifman,danjgaffney,bartdeplancke,robbie_stats,ClarissaCParker,DavidAshbrook,StatGenDan,GSCollins,MikeBradburn2,tobiaskurth,yudapearl,phuenermund,pjotrprins,GeneNetwork2,Joepdl,ewanbirney,mattmight,BioMickWatson,wait_sasha,saunaksen,dustyweb,SamHarrisOrg,timberners_lee,Atul_Gawande,OliverSacks,EricTopol,MarcusduSautoy,phylogenomics,Evolutionistrue,SCOTTeHENSLEY,lbthackray,CCDD_HSPH,nature,medrxivpreprint,ScienceMagazine,NeAnder349,HelenChuMD,dgmacarthur,JohnAllenPaulos,DrVes,NatureMedicine,KimberlyBlumen1,mbeisen,neuroconscience,firefoxx66")

;; All the repos you want to fetch commits from
(repos . '(("arvados" . "bh20-seq-resource")))

;; In the update-feed script example, sleep for 'X' amount of hours
;; after which resume updating
(refresh-time/hrs . 24)
