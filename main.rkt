#lang racket

(provide
 (rename-out [module-begin #%module-begin])
 #%datum
 #%app
 #%top
 program
 receptionists
 externals
 actors
 quote
 list
 begin
 let
 (rename-out [csa-case case])
 +
 (rename-out [csa- -])
 (rename-out [* mult])
 /
 arithmetic-shift
 (rename-out [csa= =])
 (contract-out (rename csa<  <  (-> natural-number/c natural-number/c any/c))
               (rename csa<= <= (-> natural-number/c natural-number/c any/c))
               (rename csa>  >  (-> natural-number/c natural-number/c any/c))
               (rename csa>= >= (-> natural-number/c natural-number/c any/c)))
 (rename-out [csa-and and])
 (rename-out [csa-or or])
 (rename-out [csa-not not])
 (rename-out [csa-cond cond])
 (rename-out [csa-if if])
 else
 define-state
 timeout
 send
 spawn
 goto
 goto-this-state
 define-actor
 variant
 record
 :
 list
 vector
 hash
 (rename-out [my-hash-has-key? hash-has-key?]
             [my-hash-ref hash-ref]
             [my-hash-empty? hash-empty?])
 hash-keys
 hash-values
 hash-set
 hash-remove

 ;; basic operations, for examples
 (rename-out [string-length byte-length])
 random
 cons
 list-as-variant
 list-ref
 remove
 length
 vector-length
 vector-ref
 vector-take
 vector-drop
 vector-copy
 vector-append

 ;; loops
 for/fold

 ;; for debugging only
 displayln
 printf
 )

;; ---------------------------------------------------------------------------------------------------

(require racket/async-channel
         racket/stxparam
         (for-syntax syntax/parse
                     racket/syntax))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ the-program)
     #`(#%module-begin
        (provide csa-program)
        (define csa-program the-program))]))

(define-syntax (program stx)
  (syntax-parse stx
    #:literals (receptionists externals actors)
    [(_ (receptionists [recs:id _] ...)
        (externals [exts:id _] ...)
        (actors [actor-names actor-inits] ...))
     #`(lambda (exts ...)
         (define actor-names actor-inits) ...
         (values recs ...))]))


(begin-for-syntax
  (define-syntax-class state-definition
    #:literals (timeout)
    ;; TODO: there's probably a way to get the scope for "channel" worked out without resorting to the
    ;; run-time overhead of a function call, but I don't know enough about macros to figure that out
    ;; yet
    #:attributes (transition-func-generator)
    (pattern (define-state (name:id [formal:id _] ...) (message-var:id)
               body
               (~optional [(timeout timeout-amount) timeout-body ...]))
             #:attr transition-func-generator
             (lambda (chan)
               (with-syntax ([chan chan])
                 #`(define (name formal ...)
                     (define handler-event
                       (handle-evt chan
                                   (lambda (message-var)
                                     (define transition-thunk body)
                                     (transition-thunk))))
                     (define timeout-event
                       #,(if (not (attribute timeout-amount))
                             #`never-evt
                             #`(handle-evt (alarm-evt (+ (current-inexact-milliseconds) timeout-amount))
                                           (lambda (x)
                                             (define transition-thunk (begin timeout-body ...))
                                             (transition-thunk)))))
                     (lambda ()
                       (define current-state-body
                         (lambda ()
                           (sync/timeout #f handler-event timeout-event)))
                       (current-state-thunk current-state-body)
                       (current-state-body))))))))

(define-syntax (spawn stx)
  (syntax-parse stx
    [(_ _ _ init state-def:state-definition ...)
     ;; TODO: figure out why we have to use #'init here instead of stx
     (with-syntax* ([self (datum->syntax #'init 'self)]
                    [(state-def-function ...)
                     (map (lambda (gen) (gen #'self)) (attribute state-def.transition-func-generator))])
       #'(let ()
           (define self (make-async-channel))
           (thread (lambda ()
                     state-def-function ...
                     (current-state-thunk #f)
                     (let ([transition-thunk init])
                       (transition-thunk))))
           self))]))

;; (define-keywords (func-name ...) keyword ...) defines each keyword as syntax that can only be used
;; in one of the given function names. Same for the case where (func-name ...) is replaced with
;; func-name.
(define-syntax (define-keywords stx)
  (syntax-parse stx
    [(_ function-name:id keyword ...)
     (define error-message (format "can only be used inside ~s" (syntax->datum #'function-name)))
     #`(begin
         (define-syntax (keyword stx)
           (raise-syntax-error #f #,error-message stx)) ...)]
    [(_ (function-name:id ...) keyword ...)
     (define error-message
       (format "can only be used inside one of ~s" (syntax->datum #'(function-name ...))))
     #`(begin
         (define-syntax (keyword stx)
           (raise-syntax-error #f #,error-message stx)) ...)]))

(define-keywords (spawn-agent) define-state)
(define-keywords define-state timeout)
(define-keywords program receptionists externals actors)

(define (send chan message)
  (async-channel-put chan message))

(define-syntax (goto stx)
  (syntax-parse stx
    [(_ state args ...) #`(state args ...)]))

(define current-state-thunk (make-parameter #f))

(define-syntax-rule (goto-this-state)
  (current-state-thunk))

;; ---------------------------------------------------------------------------------------------------
;; Equality

(define (csa= a b)
  (boolean->variant (equal? a b)))

(define (boolean->variant b)
  (if b (variant True) (variant False) ))

;; ---------------------------------------------------------------------------------------------------
;; Natural number operations

(match-define (list csa< csa<= csa> csa>=)
  (for/list ([op (list < <= > >=)])
    (lambda (a b)
      (if (op a b) (variant True) (variant False)))))

(define (csa- a b)
  (max 0 (- a b)))

;; ---------------------------------------------------------------------------------------------------
;; Boolean operators

(define (csa-true? a) (equal? a (variant True)))

(define (csa-and a b)
  (define a? (csa-true? a))
  (define b? (csa-true? b))
  (if (and a? b?)
      (variant True)
      (variant False)))

(define (csa-or a b)
  (define a? (csa-true? a))
  (define b? (csa-true? b))
  (if (or a? b?)
      (variant True)
      (variant False)))

(define (csa-not a)
  (if (csa-true? a) (variant False) (variant True)))

;; ---------------------------------------------------------------------------------------------------
;; Case

(define-syntax (csa-case stx)
  (syntax-parse stx
    [(_ e [(label:id field:id ...) body] ...)
     #`(let ([e-result e])
         (match e-result
           [(list 'variant 'label field ...) body] ...
           [_ (error 'csa-case
                     "No match for variant ~s; patterns were ~s"
                     e-result
                     (list '(variant label field ...) ...))]))]))

;; ---------------------------------------------------------------------------------------------------
;; Conditionals

(define-syntax (csa-cond stx)
  (syntax-parse stx #:literals (else)
    [(_ [test result1 ...+] ... [else result2 ...+])
     #`(cond
        [test result1 ...] ...
        [else result2 ...])]))

(define-syntax (csa-if stx)
  (syntax-parse stx
    [(_ test then else)
     #'(if (csa-true? test)
           then
           else)]))

;; ---------------------------------------------------------------------------------------------------

;; Actor definitions (for presentation)

(define-syntax (define-actor stx)
  (syntax-parse stx
    [(_ (actor-name args ...) init states ...)
     (syntax/loc stx
       (define (actor-name args ...)
         (spawn init states ...)))]))

;; ---------------------------------------------------------------------------------------------------
;; Data types

(define (list-as-variant l)
  (if (empty? l) (variant Empty) (variant Cons (car l) (cdr l))))

(define-syntax (hash stx)
  (syntax-parse stx
    [(_ [key val] ...)
     #`(make-immutable-hash (list (cons key val) ...))]))

(define (my-hash-has-key? h k)
  (if (hash-has-key? h k) (variant True) (variant False)))

(define (my-hash-empty? h)
  (if (hash-empty? h) (variant True) (variant False)))

(define (my-hash-ref h k)
  (match (hash-ref h k #f)
    [#f (variant Nothing)]
    [val (variant Just val)]))

(define-syntax (variant stx)
  (syntax-parse stx
    [(_ tag fields ...)
     #'(list 'variant 'tag fields ...)]))

(define-syntax (record stx)
  (syntax-parse stx
    [(_ [field-label:id field-val] ...)
     #`(make-immutable-hash (list (cons 'field-label field-val) ...))]))

(define-syntax (: stx)
  (syntax-parse stx
    [(_ rec field:id)
     #`(hash-ref rec 'field)]))
