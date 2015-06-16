#lang csa

;; A simple forwarding agent that sends its input to the most recently assigned output address if in
;; the ON state, or discards the input if in the OFF state

(provide Switch)

(define (Switch out)
  (spawn
   (goto On out)
   (define-state (Off out) (e)
     (match e
       [(list 'In m) (goto Off out)]
       ['Toggle (goto On out)]
       [(list 'NewOut c) (goto Off c)]))
   (define-state (On out) (e)
     (match e
       [(list 'In m)
        (send out m)
        (goto On out)]
       ['Toggle (goto Off out)]
       [(list 'NewOut c) (goto On c)]))))

;; ---------------------------------------------------------------------------------------------------
;; Specification

(spec
 (goto Off initial-out)
 (define-state (Off current-out)
   [(list 'In *) -> (goto On current-out)]
   ['Toggle -> (goto On current-out)]
   [(list 'NewOut c) -> (goto Off c)])
 (define-state (On current-out)
   [(list 'In *) -> (with-outputs ([current-out *]) (goto On current-out))]
   ['Toggle -> (goto Off current-out)]
   [(list 'NewOut c) -> (goto On c)]))
