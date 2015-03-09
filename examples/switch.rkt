#lang csa

(provide Switch)

(define-variant-type Unit [UnitValue])

(define (Switch out)
  (spawn-agent
   (([in Nat]
     [toggle Unit]
     [new-out (ChannelOf Nat)])
    (goto On out)
    (define-state (Off [out (ChannelOf Nat)])
      [in (m) (goto Off out)]
      [toggle (u) (goto On out)]
      [new-out (c) (goto Off c)])
    (define-state (On [out (ChannelOf Nat)])
      [in (m)
        (send out m)
        (goto On out)]
      [toggle (u) (goto Off out)]
      [new-out (c) (goto On c)]))
   (list in toggle new-out)))

;; ---------------------------------------------------------------------------------------------------
;; Specification

(define-spec SwitchSpec
  (channels [in Nat] [toggle Unit] [new-out (ChannelOf Nat)])
  (define-state (Off [current-out (ChannelOf Nat)])
    [in * -> (On current-out)]
    [toggle * -> (On current-out)]
    [new-out c -> (Off c)])
  (define-state (On [current-out (ChannelOf Nat)])
    [in * -> (On current-out) (out [current-out *])]
    [toggle * -> (Off current-out)]
    [new-out c -> (On c)]))
