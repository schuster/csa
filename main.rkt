#lang racket

(provide
 (rename-out [my-module-begin #%module-begin])
 #%datum
 #%app
 #%top
 provide
 define ; only used to provide a constructor for tests
 list ; only used to provide constructor return value
 begin
 +
 -
 (contract-out (rename my= = (-> natural-number/c natural-number/c any/c))
               (rename my< < (-> natural-number/c natural-number/c any/c)))
 define-state
 timeout
 send
 spawn-agent
 goto
 goto-this-state
 define-variant-type
 case

 ;; type keywords
 Bool
 Nat
 ChannelOf

 ;; specifications
 define-spec

 ;; for debugging only
 displayln
 printf
 )

;; ---------------------------------------------------------------------------------------------------

(require racket/async-channel
         (for-syntax syntax/parse
                     redex/reduction-semantics
                     "model.rkt"))

(begin-for-syntax
  (define-syntax-class type
   #:literals (Bool Nat ChannelOf)
   (pattern Bool)
   (pattern Nat)
   (pattern name:id)
   (pattern (ChannelOf t:type)))

  (define-syntax-class state-definition
   #:attributes (transition-func)
   (pattern (define-state (name:id [formal:id formal-type:type] ...) handler:message-handler ...)
            #:attr transition-func
            #`(define (name formal ...)
                (lambda ()
                  (define current-state-body (lambda () (sync/timeout #f handler.event ... never-evt)))
                  (current-state-thunk current-state-body)
                  (current-state-body)))))

 (define-syntax-class message-handler
   #:literals (timeout)
   #:attributes (event)
   (pattern (channel:id (message-var:id) body ...)
            #:attr event
            #'(handle-evt channel
                        (lambda (message-var)
                          (define transition-thunk (begin body ...))
                          (transition-thunk))))
   (pattern ((timeout timeout-amount:nat) body ...)
            #:attr event
            #'(handle-evt (alarm-evt (+ (current-inexact-milliseconds) (* 1000 timeout-amount)))
                          (lambda (x)
                            (define transition-thunk (begin body ...))
                            (transition-thunk))))))

(define-syntax-rule (my-module-begin body ...)
  (#%module-begin body ... (sync never-evt)))

(define-syntax (spawn-agent stx)
  (syntax-parse stx
    [(_ (([chan:id chan-type:type] ...) state-def:state-definition ... init) body ...)
     #'(let ()
         (define chan (make-async-channel)) ...
         (thread (lambda ()
                   state-def.transition-func ...
                   (current-state-thunk #f)
                   (let ([transition-thunk init])
                     (transition-thunk))))
         body ...)]))

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

(define-keywords (spawn-agent define-spec) channels define-state)
(define-keywords (spawn-agent define-variant-type) Bool Nat ChannelOf)
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
;; Variant types

(define-syntax (define-variant-type stx)
  (syntax-parse stx
    [(_ name [variant-name arg-type ...] ...)
     ;; TODO: figure out a better way to generate one fresh variable per arg-type
     (with-syntax
         ([((arg ...) ...)
                    (map (lambda (arg-list) (map (lambda (arg) (gensym)) (syntax->list arg-list)))
                         (syntax->list #'((arg-type ...) ...)))])
       #'(begin
           (define (variant-name arg ...) (list 'variant-name arg ...)) ...))]))

(define-syntax (case stx)
  (syntax-parse stx
    [(_ t [variant-name (args ...) body ...] ...)
     #'(match t
         [(list 'variant-name args ...) body ...] ...)]))

;; ---------------------------------------------------------------------------------------------------
;; Specifications

(define-syntax (define-spec stx)
  (unless (redex-match aps D (syntax->datum stx))
    (raise-syntax-error #f "Invalid syntax for specification" stx))
  #'(void))

;; ---------------------------------------------------------------------------------------------------
;; Natural number operations

;; Returns the variant (True) or (False)
(define (my= a b)
  (if (= a b) (list 'True) (list 'False)))

(define (my< a b)
  (if (< a b) (list 'True) (list 'False)))

