#lang racket/base

(require racket/port
         rackunit
         threading
         html-parsing
         "../web/common.rkt"
         "../web/arxiv.rkt")

(define test-xml-content
  (~> "tests/test-arxiv-content.html"
      open-input-file
      port->string
      html->xexp))

(check-equal? (parse-arxiv-search-terms '()
                                        0
                                        '((AND COVID-19 title)
                                          (OR SARS-CoV-2 abstract)
                                          (OR COVID-19 abstract)
                                          (OR SARS-CoV-2 title)
                                          (OR coronavirus title)
                                          (OR coronavirus abstract)))
              '((terms-0-operator . "AND")
                (terms-0-term . "COVID-19")
                (terms-0-field . "title")
                (terms-1-operator . "OR")
                (terms-1-term . "SARS-CoV-2")
                (terms-1-field . "abstract")
                (terms-2-operator . "OR")
                (terms-2-term . "COVID-19")
                (terms-2-field . "abstract")
                (terms-3-operator . "OR")
                (terms-3-term . "SARS-CoV-2")
                (terms-3-field . "title")
                (terms-4-operator . "OR")
                (terms-4-term . "coronavirus")
                (terms-4-field . "title")
                (terms-5-operator . "OR")
                (terms-5-term . "coronavirus")
                (terms-5-field . "abstract")))

(check-equal? (map-xexp test-xml-content
                        (string-append "//li[contains(@class, "
                                       "'arxiv-result')]")
                        sxpath->feed-struct/arxiv)
                (list
                 (feed-arxiv
                  "Timely Tracking of Infection Status of Individuals in a Population"
                  "Melih Bastopcu, Sennur Ulukus"
                  "We consider real-time timely tracking of infection status (e.g., - ) of individuals in a population. In this work, a health care provider wants to detect infected people as well as people who recovered from the disease as quickly as possible. In order to measure the timeliness of the tracking process, we use the"
                  "24 December, 2020"
                  "https://arxiv.org/abs/2012.13393"
                  "-3836862324113911226")
                 (feed-arxiv
                  "Simple mathematical models for controlling COVID-19 transmission through social distancing and community awareness"
                  "Ahmed S. Elgazzar"
                  "The novel -"
                  "24 December, 2020"
                  "https://arxiv.org/abs/2012.13361"
                  "452229514406158235"))
              "Check correct struct")
