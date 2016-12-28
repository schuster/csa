#lang racket

(provide csa-run)

(define-namespace-anchor outer-module)

;; Starts the given program (a quoted version of the "real" program), returning the address for each
;; receptionist as multiple values
(define (csa-run prog)
  ;; Create a new empty namespace that still shares the module registry with this one so that all
  ;; structs, etc. (especially async-channel) are "the same" as those used in csa-run's caller
  (parameterize ([current-namespace (namespace-anchor->empty-namespace outer-module)])
    (namespace-require 'csa)
    ((eval prog))))
