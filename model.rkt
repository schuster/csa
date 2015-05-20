#lang racket

;; Models the grammar for APS (Agent Protocol Specifications)

(provide aps)

;; ---------------------------------------------------------------------------------------------------

(require redex/reduction-semantics)

(define-language csa
  ;; begin, case, goto, spawn
  (f (begin e ... f)
     (match [p f] ...)
     (goto s e ...)
     (spawn-agent (c f S ...) f))
  (S (define-state (s x ...) f ...)
     (define-state (s x ...) f ... [(timeout n) f]))
  (e x
     c
     n
     (b e ...)
     y
     (list e ...)
     (send e e))
  (p x
     y
     (list p ...))
  (b + - < =)
  ((x c s) variable-not-otherwise-mentioned)
  (y (quote variable-not-otherwise-mentioned))
  (n natural))

(define-language aps
  (D (define-spec d
       (define-state (s-hat c-hat ...) R ...) ...))
  ;; differs from the paper; R is a state clause
  (R [pi -> (s-hat e-hat ...) (out o ...) (activ π ...)]
     [unobs -> (s-hat e-hat ...) (out o ...) (activ π ...)]

     ;; Shorthands
     [pi -> (s-hat e-hat ...) (out o ...)]
     [pi -> (s-hat e-hat ...) (activ π ...)]
     [pi -> (s-hat e-hat ...)]
     [unobs -> (s-hat e-hat ...) (out o ...)]
     [unobs -> (s-hat e-hat ...) (activ π ...)]
     [unobs -> (s-hat e-hat ...)])
  (o [e-hat po])
  (π [c-hat d (s-hat e-hat ...)])
  (e-hat c-hat)
  (pi *
      y
      (list pi ...)
      c-hat)
  (po *
      y
      (list po ...)
      (spec-chan c-hat)
      (spec-chan self))
  (y (quote variable-not-otherwise-mentioned))

  ;; Names
  (d variable-not-otherwise-mentioned)
  (s-hat variable-not-otherwise-mentioned)

  ;; Should come from the PL
  (c-hat variable-not-otherwise-mentioned))
