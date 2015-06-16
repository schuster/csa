#lang racket

;; Models the grammar for APS (Agent Protocol Specifications)

(provide aps)

;; ---------------------------------------------------------------------------------------------------

(require redex/reduction-semantics)

(define-language csa
  (e (spawn e S ...)
     (goto s e ...)
     (send e e)
     self
     (begin e ... e)
     (let ([x e] ...) e)
     (match [p e] ...)
     (list e ...)
     t
     x
     n)
  (S (define-state (s x ...) e)
     (define-state (s x ...) e [(timeout n) e]))
  (p *
     x
     t
     (list p ...))
  ((x s) variable-not-otherwise-mentioned)
  (t (quote variable-not-otherwise-mentioned))
  (n natural))

(define-extended-language aps
  csa
  (e-hat (let-spec (x (goto s u ...) S-hat ...) e-hat)
         (goto s u ...)
         (with-outputs ([u po] ...) e-hat))
  (S-hat (define-state (s x ...) (ε -> e-hat) ...))
  (ε unobs
     p)
  (u x) ; arguments
  (po *
      x
      self
      t
      (list po ...)))
