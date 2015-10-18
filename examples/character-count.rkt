#lang csa

;; just writing this in pseudocode for now

(provide CharacterCounter)

(define (CharacterCounter)
  (spawn
   (goto Init)
   (define-state (Init) (m)
     (match m
       [(list 'NewTarget t) (goto Running t)]
       [* (goto Init)]))
   (define-state (Forwarding target) (m)
     (match m
       [(list 'Message s)
        (send target (list 'WrappedMessage (length s) s))
        (goto Forwarding target)]
       [(list 'NewTarget new-target) (goto Forwarding new-target)]
       ['Suspend (goto Suspended target)]
       [* (goto Forwarding target)]))
   (define-state (Suspended target) (m)
     (begin
       (match m
         [(list 'NewTarget new-target) (goto Forwarding new-target)]
         ['Resume (goto Forwarding target)]
         [* (goto Suspended target)])))))

;; ---------------------------------------------------------------------------------------------------
;; Specification

(spec
 (goto Init)
 (define-state (Init)
   [(list 'Message String) -> (goto On t)]
   [(list 'NewTarget t) -> (goto Off t)])
 (define-state (Running t)
   [(list 'Message *) -> (with-outputs ([t (list 'WrappedMessage * *)]) (goto Running t))]
   [(list 'Message *) -> (goto Running t)]
   [(list 'NewTarget new-t) -> (goto Running new-t)]))

;; ---------------------------------------------------------------------------------------------------
;; Possible type ideas

#|

NOTEs:

* key issue for types: either the worker OR the dispatcher in, e.g., the book recs example should be able to send the OK message with worker address *without* changing the type of the dispatcher

|#


#|

type for switch:
letrec
 Init() =
  ?(Msg).INit()
  | ?(Chan(c)).On(c);
 On(c) =
   ?(Msg).c!(msg).On(c);
  |?(Chan(c')).On(c');
in Init()

# Maybe I can prove this type/spec through non-algorithmic type-checking

|#
