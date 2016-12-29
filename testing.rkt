#lang racket

(provide
 csa-record
 csa-record*
 csa-variant)

(require (for-syntax syntax/parse))

(define-match-expander csa-record
  (lambda (stx)
    (syntax-parse stx
      [(_ (field-name val-pattern) ...)
       #`(hash-table ('field-name val-pattern) ...)])))

(define-match-expander csa-record*
  (lambda (stx)
    (syntax-parse stx
      [(_ (field-name val-pattern) ...)
       #`(hash-table ('field-name val-pattern) ... (_ _) (... ...))])))

(define-match-expander csa-variant
  (lambda (stx)
    (syntax-parse stx
      [(_ tag:id vals ...)
       #`(quote (variant tag (unquote vals) ...))])))
