#lang csa

;; just writing this in pseudocode for now

(provide ByteCounter)

(define (ByteCounter)
  (spawn
   (goto Init)
   (define-state (Init) (m)
     (match m
       [(list 'NewTarget t) (goto Forwarding t)]
       [* (goto Init)]))
   (define-state (Forwarding target) (m)
     (match m
       [(list 'Payload s)
        (send target (list 'Packet (length s) s))
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
   [(list 'Payload String) -> (goto On t)]
   [(list 'NewTarget t) -> (goto Off t)])
 (define-state (Running t)
   [(list 'Payload *) ->
    (with-outputs ([t (list 'Packet * *)])
      (goto Running t))]
   [(list 'Payload *) -> (goto Running t)]
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
