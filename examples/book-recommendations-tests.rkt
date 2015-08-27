#lang racket

(require "book-recommendations.rkt"
         (only-in csa spawn)
         rackunit
         asyncunit
         racket/async-channel)

(test-case
 "Check full protocol process"
 (define user (make-async-channel))
 (define web-service (spawn WebService))
 (async-channel-put web-service (list 'Req "TAPL" user))
 (define worker (check-unicast-match user (list 'Ok worker) #:result worker))
 (check-unicast user (list 'Done
                           (list "Advanced Topics in Types and Programming Languages"
                                 "Semantics Engineering with PLT Redex"
                                 "The Formal Semantics of Programming Languages")))
 (async-channel-put worker 'MoreRecs)
 (check-unicast user (list 'Done
                           (list "Advanced Topics in Types and Programming Languages"
                                 "Semantics Engineering with PLT Redex"
                                 "The Formal Semantics of Programming Languages"))))

