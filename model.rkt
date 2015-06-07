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
     (b e ...)
     t
     x
     n)
  (S (define-state (s x ...) e)
     (define-state (s x ...) e [(timeout n) e]))
  (p *
     x
     t
     (list p ...))
  (b + - < =)
  ((x s) variable-not-otherwise-mentioned)
  (t (quote variable-not-otherwise-mentioned))
  (n natural))

(define-extended-language aps
  csa
  (Σ ((goto s x ...) S-hat ...)
     ;; (define-spec d
     ;;      (define-state (s-hat c-hat ...) R ...) ...)
     )
  (e-hat (goto s x ...)
         (with-outputs ([x po] ...) e-hat)
         (let-specs ([x Σ] ...) e-hat))
  (S-hat (define-state (s x ...) (ε -> e-hat) ...))
  (ε unobs
     p)
  (po *
      x
      self
      t
      (list po ...)))
