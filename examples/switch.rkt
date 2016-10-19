#lang csa

;; A simple forwarding agent that sends its input to the most recently assigned output address if in
;; the ON state, or discards the input if in the OFF state

(program
 (receptionists [switch (Union (In Nat) (Toggle) (NewOut (Addr Nat)))])
 (externals [out Nat])
 (actors
  [switch
   (let ()
     (spawn switch-spawn (Union (In Nat) (Toggle) (NewOut (Addr Nat)))
       (goto On out)

       (define-state (Off [out (Addr Nat)]) (e)
         (case e
           [(In m) (goto Off out)]
           [(Toggle) (goto On out)]
           [(NewOut c) (goto Off c)]))

       (define-state (On [out (Addr Nat)]) (e)
         (case e
           [(In m)
            (begin
              (send out m)
              (goto On out))]
           [(Toggle) (goto Off out)]
           [(NewOut c) (goto On c)]))))]))
