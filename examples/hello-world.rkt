#lang csa

(spawn-agent
 (([in Nat])
  (define-state (Ready)
    [in (m)
      (printf "Hello, world!\n")
      (goto Halt)])
  (define-state (Halt)
    [in (m)
      (goto Halt)])
  (goto Ready))
 (send in 0))
