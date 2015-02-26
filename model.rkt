#lang racket

;; Models the grammar for APS (Agent Protocol Specifications)

(provide aps)

;; ---------------------------------------------------------------------------------------------------

(require redex/reduction-semantics)

(define-language aps
  (D (define-spec d
       (channels [c-hat τ] ...)
         (define-state (s-hat [c-hat τ] ...) R ...) ...))
  ;; differs from the paper; R is a state clause
  (R [c-hat pi -> (s-hat e-hat ...) (out o ...) (activ π ...)]
     [unobs -> (s-hat e-hat ...) (out o ...) (activ π ...)]

     ;; Shorthands
     [c-hat pi -> (s-hat e-hat ...) (out o ...)]
     [c-hat pi -> (s-hat e-hat ...) (activ π ...)]
     [c-hat pi -> (s-hat e-hat ...)]
     [unobs -> (s-hat e-hat ...) (out o ...)]
     [unobs -> (s-hat e-hat ...) (activ π ...)]
     [unobs -> (s-hat e-hat ...)])
  (o [e-hat po])
  (π [f d (s-hat e-hat ...)])
  (e-hat c-hat)
  (pi *
      (V pi ...)
      c-hat)
  (po *
      (V po ...)
      (spec-chan σ c-hat)
      (spec-chan self c-hat))

  ;; Names
  (d variable-not-otherwise-mentioned)
  (σ variable-not-otherwise-mentioned)
  (s-hat variable-not-otherwise-mentioned)

  ;; Should come from the PL
  (τ Bool
     Nat
     T
     (ChannelOf τ))
  (V variable-not-otherwise-mentioned)
  (c-hat variable-not-otherwise-mentioned)
  (T variable-not-otherwise-mentioned))
