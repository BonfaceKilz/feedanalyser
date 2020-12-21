#lang racket/base

(require racket/port
         rackunit
         threading
         "../web/pubmed.rkt")

(define test-html-content
  (port->string
   (open-input-file "tests/test-pubmed-content.html") #:close? #t))


(check-equal? (extract-content/articles test-html-content)
              (list
               (feed-pubmed
                "Afzali M, Ryazantsev SV, Shakeri A."
                "Afzali M, et al."
                (string-append
                 "Probl Sotsialnoi Gig Zdravookhranenniiai "
                 "Istor Med. 2020 Nov;28(6):1231-1239. "
                 "doi: 10.32687/0869-866X-2020-28-6-1231-1239.")
                "Probl Sotsialnoi Gig Zdravookhranenniiai Istor Med. 2020."
                (string-append
                 "[The psychological impacts of quarantine on "
                 "international students' life satisfaction "
                 "in Russia during coronavirus COVID-19].")
                "33338331")
               (feed-pubmed
                (string-append
                 "Kakoullis L, Eliades E, Papachristodoulou E,"
                 " Parperis K, Chra P, Constantinidou A, "
                 "Chatzittofis A, Sampsonas F, Panos G.")
                "Kakoullis L, et al."
                (string-append
                 "Int J Clin Pract. "
                 "2020 Dec 18:e13944. doi: 10.1111/ijcp.13944. "
                 "Online ahead of print.")
                "Int J Clin Pract. 2020."
                "Response to COVID-19 in Cyprus: policy changes and epidemic trends."
                "33338320"))
              "Check correct struct")
