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
   (define-state (Running target) (m)
     (match m
       [(list 'Message s)
        (send target (list 'WrappedMessage (length s) s))
        (goto Running target)]
       [(list 'NewTarget new-target) (goto Running new-target)]
       ['Suspend (goto Suspended target)]
       [* (goto Running target)]))
   (define-state (Suspended target) (m)
     (begin
       (match m
         [(list 'NewTarget new-target) (goto Running new-target)]
         ['Resume (goto Running target)]
         [* (goto Suspended target)])))))

;; ---------------------------------------------------------------------------------------------------
;; Specification

(spec
 (goto Off initial-out)
 (define-state (Init)
   [(list 'Message String) -> (goto On current-out)]
   [(list 'NewOut t) -> (goto Off t)])
 (define-state (On t)
   [(list 'In *) -> (with-outputs ([current-out (list 'WrappedMessage * *)]) (goto On current-out))]
   [(list 'In *) -> (goto On current-out)]
   [(list 'NewOut c) -> (goto On c)]))

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
