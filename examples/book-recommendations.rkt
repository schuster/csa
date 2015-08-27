#lang csa

(provide WebService)

(define-actor (WebService)
  (goto Ready)
  (define-state (Ready) (m)
    (match m
      [(list 'Req book user)
       (let ([worker (spawn Worker book user)])
         (send user (list 'Ok worker))
         (goto Ready))]
      [* (goto Ready)])))

(define-actor (Worker book user)
  (begin
    (spawn Tagger book self)
    (goto AwaitTags))
  (define-state (AwaitTags) (m)
    (match m
      [(list 'Tags book-tags)
       (spawn RecommendationEngine book-tags self)
       (goto AwaitIsbns)]
      [* (goto AwaitTags)]))
  (define-state (AwaitIsbns) (m)
    (match m
      [(list 'Recs recommendation-isbns)
       (spawn IsbnLookup recommendation-isbns self)
       (goto AwaitData)]
      [* (goto AwaitIsbns)]))
  (define-state (AwaitData) (m)
    (match m
      [(list 'Data recommendation-data)
       (send user (list 'Done recommendation-data))
       (goto Done)]
      [* (goto AwaitData)]))
  (define-state (Done) (m)
    (match m
      ['MoreRecs
       (spawn Tagger book self)
       (goto AwaitTags)]
      [* (goto Done)])))

;; ---------------------------------------------------------------------------------------------------
;; Dummy implementations

(define-actor (Tagger book worker)
  (begin
    (send worker (list 'Tags (list "PL" "types" "semantics")))
    (goto Done))
  (define-state (Done) (m) (goto Done)))

(define-actor (RecommendationEngine tags worker)
  (begin
    (send worker (list 'Recs (list "ISBN1" "ISBN2" "ISBN3")))
    (goto Done))
  (define-state (Done) (m) (goto Done)))

(define-actor (IsbnLookup isbns worker)
  (begin
    (send worker (list 'Data (list "Advanced Topics in Types and Programming Languages"
                                   "Semantics Engineering with PLT Redex"
                                   "The Formal Semantics of Programming Languages")))
    (goto Done))
  (define-state (Done) (m) (goto Done)))

;; ---------------------------------------------------------------------------------------------------
;; Specificiation

(define WebServiceSpec
  (spec
   (goto Ready)
   (define-state (Ready)
     [(list 'Req * user) ->
      (let-spec (worker
                 (goto Working)
                 (define-state (Working user)
                   [unobs -> (with-outputs ([user (list 'Done *)]) (goto Done user))]
                   ['MoreRecs -> (goto Working user)])
                 (define-state (Done user)
                   ['MoreRecs -> (goto Working user)]))
        (with-outputs ([user (list 'Ok worker)])
          (goto Ready)))])))
