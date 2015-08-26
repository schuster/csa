#lang racket

(provide
 #%module-begin
 #%datum
 #%app
 #%top
 provide
 define ; only used to provide a constructor for tests
 quote
 list
 begin
 let
 (rename-out [csa-match match])
 +
 (rename-out [csa- -])
 (contract-out (rename csa= = (-> natural-number/c natural-number/c any/c))
               (rename csa< < (-> natural-number/c natural-number/c any/c)))
 (rename-out [csa-cond cond])
 else
 define-state
 timeout
 send
 spawn
 spawn-named-agent
 goto
 goto-this-state
 define-actor

 ;; specifications
 spec

 ;; for debugging only
 displayln
 printf
 )

;; ---------------------------------------------------------------------------------------------------

(require racket/async-channel
         (for-syntax syntax/parse
                     racket/syntax
                     redex/reduction-semantics
                     "model.rkt"))

(begin-for-syntax
  (define-syntax-class state-definition
    #:literals (timeout)
    ;; TODO: there's probably a way to get the scope for "channel" worked out without resorting to the
    ;; run-time overhead of a function call, but I don't know enough about macros to figure that out
    ;; yet
    #:attributes (transition-func-generator)
    (pattern (define-state (name:id formal:id ...) (message-var:id)
               body
               (~optional [(timeout timeout-amount:nat) timeout-body ...]))
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
                             #`(handle-evt (alarm-evt (+ (current-inexact-milliseconds) (* 1000 timeout-amount)))
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
    [(_ name:id args ...)
     (syntax/loc stx (name args ...))]
    [(_ init state-def:state-definition ...)
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

(define-syntax (spawn-named-agent stx)
  (syntax-parse stx
    [(_ (agent:id arg ...))
     #'(agent arg ...)]))

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

(define-keywords (spawn-agent define-spec) define-state)
(define-keywords define-state timeout)

(define (send chan message)
  (async-channel-put chan message))

(define-syntax (goto stx)
  (syntax-parse stx
    [(_ state args ...) #`(state args ...)]))

(define current-state-thunk (make-parameter #f))

(define-syntax-rule (goto-this-state)
  (current-state-thunk))

;; ---------------------------------------------------------------------------------------------------
;; Specifications

(define-syntax (spec stx)
  (syntax-parse stx
    [(_ spec-contents ...)
     (unless (redex-match aps ((goto s u ...) S-hat ...) (syntax->datum #'(spec-contents ...)))
       (raise-syntax-error #f "Invalid syntax for specification" stx))
     #'(void)]))

;; ---------------------------------------------------------------------------------------------------
;; Natural number operations

(define (csa= a b)
  (if (= a b) 'True 'False))

(define (csa< a b)
  (if (< a b) 'True 'False))

(define (csa- a b)
  (max 0 (- a b)))

;; ---------------------------------------------------------------------------------------------------
;; Pattern matching

(begin-for-syntax
  (define-syntax-class match-pattern
    #:literals (list quote)
    (pattern x:id)
    (pattern (quote s:id))
    (pattern (list p:match-pattern ...))))

(define-syntax (csa-match stx)
  (syntax-parse stx
    [(_ e [pat:match-pattern body ...+] ...)
     #'(match e
         [pat body ...] ...
         [_ (sync)])])) ; A match without a matching clause is a stuck state

;; ---------------------------------------------------------------------------------------------------
;; Conditionals

(define-syntax (csa-cond stx)
  (syntax-parse stx #:literals (else)
    [(_ [test result1 ...+] ... [else result2 ...+])
     #`(cond
        [test result1 ...] ...
        [else result2 ...])]))

;; ---------------------------------------------------------------------------------------------------

;; Actor definitions (for presentation)

(define-syntax (define-actor stx)
  (syntax-parse stx
    [(_ (actor-name args ...) init states ...)
     (syntax/loc stx
       (define (actor-name args ...)
         (spawn init states ...)))]))
