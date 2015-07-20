#lang racket

;; Models the grammar for APS (Agent Protocol Specifications)

(provide csa
         csa-eval
         inject-message
         handler-step
         aps
         aps-eval)

;; ---------------------------------------------------------------------------------------------------

(require redex/reduction-semantics)

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
;; CSA

(define-language csa
  (e (spawn e S ...)
     (goto s e ...)
     (send e e)
     self
     (begin e ... e)
     (let ([x e] ...) e)
     (match e [p e] ...)
     (list e ...)
     t
     x
     n)
  (S (define-state (s x ...) (x) e)
     (define-state (s x ...) (x) e [(timeout n) e]))
  (p *
     x
     t
     (list p ...))
  ((x s) variable-not-otherwise-mentioned)
  (t (quote variable-not-otherwise-mentioned))
  (n natural))

(define-extended-language csa-eval
  csa
  (K (α μ ρ χ))
  (α ((a ((S ...) e)) ...))
  (μ (m ...))
  (m (a <= v))
  ((ρ χ) (a ...))
  (e ....
     a
     (rcv (x) e)
     (rcv (x) e [(timeout n) e]))
  (v n t (list v ...) a)
  (a (addr natural))
  (A ((any_1 ... hole any_2 ...) μ ρ χ))
  (E hole
     (goto s v ... E e ...)
     (send E e)
     (send v E)
     (begin E e ...)
     (let ([x E] [x e] ...) e)
     (match E [p e] ...)
     (list v ... E e ...)))

(define handler-step
  (reduction-relation csa-eval
    #:domain K
    (--> (in-hole A (a ((S ...) (in-hole E (goto s v ..._n)))))
         (in-hole A (a ((S ...) (rcv (x_h) (subst-n e (x_s v) ...)))))
         (where (_ ... (define-state (s x_s ..._n) (x_h) e) _ ...) (S ...))
         Goto)

    ;; TODO: goto with timeout

    ;; let, match, begin, send, goto
    (==> (begin v e e_rest ...)
         (begin e e_rest ...)
         Begin1)
    (==> (begin v)
         v
         Begin2)
    ;; TODO: send
    ;; TODO: let
    ;; TODO: match

    with
    [(--> (in-hole A (a ((S ...) (in-hole E e_old))))
          (in-hole A (a ((S ...) (in-hole E e_new)))))
     (==> e_old e_new)]))

(module+ test
  (define empty-A-context (term ((hole) () () ())))
  (define S1-def (term (define-state (S1 a b) (x) (begin a x (goto S1 b a)))))
  (check-not-false (redex-match csa-eval S S1-def))
  (check-not-false (redex-match csa-eval A empty-A-context))
  (define init-config
    (term (in-hole ,empty-A-context ((addr 1) ((,S1-def) (begin 'foo 'baz (goto S1 'bar 'foo)))))))
  (check-not-false (redex-match csa-eval K init-config))
  ;; begin1
  ;; begin2
  ;; goto
  ;; all the way through a goto, with begins

  (check-equal?
   (apply-reduction-relation* handler-step
                              init-config)
   (list (term (in-hole ,empty-A-context
                        ((addr 1) ((,S1-def) (rcv (x) (begin 'bar x (goto S1 'foo 'bar))))))))))

(define-metafunction csa-eval
  subst-n : e (x v) ... -> e
  [(subst-n e) e]
  [(subst-n e (x v) any_rest ...)
   (subst-n (subst e x v) any_rest ...)])

(define-metafunction csa-eval
  subst : e x v -> e
  [(subst x x v) v]
  [(subst x x_2 v) x]
  [(subst n x v) n]
  [(subst t x v) t]
  [(subst a x v) a]
  [(subst (spawn e S ...) self v) (spawn e S ...)]
  [(subst (spawn e S ...) x v)
    (spawn (subst e x v) (subst/S S x v) ...)]
  [(subst (goto s e ...) x v) (goto s (subst e x v) ...)]
  [(subst (send e_1 e_2) x v)
   (send (subst e_1 x v) (subst e_2 x v))]
  [(subst (begin e ...) x v) (begin (subst e x v) ...)]
  [(subst (let ([x_let e] ...) e_body) x v)
   (let ([x_let (subst e x v)] ...) e_body)
   (where (_ ... x _ ...) (x_let ...))] ; check that x is in the list of bound vars
  [(subst (let ([x_let e] ...) e_body) x v)
   (let ([x_let (subst e x v)] ...) (subst e_body x v))]
  [(subst (match e [p e_pat] ...) x v)
   (match (subst e x v) (subst/match-clause [p e_pat] x v) ...)]
  [(subst (list e ...) x v) (list (subst e x v) ...)]
  [(subst (rcv (x) e) x v) (rcv (x) e)]
  [(subst (rcv (x_h) e) x v) (rcv (x_h) (subst e x v))]
  [(subst (rcv (x) e [(timeout n) e_timeout]) x v) (rcv (x) e [(timeout n) e_timeout])]
  [(subst (rcv (x_h) e [(timeout n) e_timeout]) x v)
   (rcv (x_h) (subst e x v) [(timeout n) (subst e_timeout x v)])])

(define-metafunction csa-eval
  subst/match-clause : [p e] x v -> [p e]
  [(subst/match-clause [p e] x v)
   [p e]
   (side-condition (term (pattern-binds-var p x)))]
  [(subst/match-clause [p e] x v)
   [p (subst e x v)]])

(define-metafunction csa-eval
  pattern-binds-var : p x -> boolean
  [(pattern-binds-var * x) #f]
  [(pattern-binds-var x x) #t]
  [(pattern-binds-var x_1 x_2) #f]
  [(pattern-binds-var t x) #t]
  [(pattern-binds-var (list p ...) x)
   ,(ormap values (term ((pattern-binds-var p x) ...)))])

(module+ test
  (check-equal? (term (subst/match-clause [(list x y z) x] x 'foo))
                (term [(list x y z) x]))
  (check-equal? (term (subst/match-clause [(list a y z) x] x 'foo))
                (term [(list a y z) 'foo])))

(define-metafunction csa-eval
  subst/S : S x v -> S
  [(subst/S (define-state (s x_s ...) (x_h) e) x v)
   (define-state (s x_s ...) (x_h) e)
   (where (_ ... x _ ...) (x_s ... x_h))]
  [(subst/S (define-state (s x_s ...) (x_h) e) x v)
   (define-state (s x_s ...) (x_h) (subst e x v))]
  [(subst/S (define-state (s x_s ...) (x_h) e_1 [(timeout n) e_2]) x v)
   (define-state (s x_s ...) (x_h) e_1 [(timeout n) e_2])
   (where (_ ... x _ ...) (x_s ... x_h))]
  [(subst/S (define-state (s x_s ...) (x_h) e_1 [(timeout n) e_2]) x v)
   (define-state (s x_s ...) (x_h) (subst e_1 x v) [(timeout n) (subst e_2 x v)])])

(module+ test
  (check-equal? (term (subst 'foo x 'bar))
                (term 'foo))
  (check-equal? (term (subst a a 'foo))
                (term 'foo))
  (check-equal? (term (subst a b 'foo))
                (term a))
  (check-equal? (term (subst (goto s x y) x 'foo))
                (term (goto s 'foo y)))
  (check-equal? (term (subst (begin x y x) x 'foo))
                (term (begin 'foo y 'foo)))

  (check-equal? (term (subst-n (goto s x y z) (x 'foo) (y 'bar)))
                (term (goto s 'foo 'bar z)))
  ;; TODO: more tests
  )

;; Substitutes an external message into the config. Will throw an error if the address is not in the
;; set of receptionists.
(define-metafunction csa-eval
  inject-message : K a v -> K
  ;; TODO: do the case for rcv with timeout, too
  [(inject-message ((any_1 ... (a ((S ...) (rcv (x) e))) any_2 ...) μ (a_1 ... a a_2 ...) χ) a v)
   ((any_1 ... (a ((S ...) (subst e x v))) any_2 ...) μ (a_1 ... a a_2 ...) χ)]
  [(inject-message (_ _ ρ _) a v)
   (side-condition (not (member (term ρ) (term a))))
   (side-condition (error "Address ~s is not a receptionist address" (term a)))])

;; ---------------------------------------------------------------------------------------------------
;; APS

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

(define-extended-language aps-eval
  aps
  (z ((S-hat ...) e-hat σ))
  (σ a null)
  (u .... a))
