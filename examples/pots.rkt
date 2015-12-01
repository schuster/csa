#lang csa

;; An implementation of the POTS system often used as an Erlang tutorial, found here:
;; https://github.com/uwiger/pots

;; Difference's from Ulf's version:
;; * all messages other than number analysis are send-and-forget
;; * any response to a request is assumed to be for the most recent request (i.e. no "Ref"s)

;; Notes on the protocol:
;; * regardless of who started the call, the side that hangs up first sends Disconnect to the LIM
;; * "A" side is the calling side, "B" side is the called side

(provide PotsAgent)

;; ---------------------------------------------------------------------------------------------------

(define (PotsAgent lim analyzer)
  (spawn
   (goto Idle lim analyzer)

   (define-state (Idle lim analyzer) (m)
     (match m
       ['OffHook
        (send lim (list 'StartTone 'Dial))
        (goto GettingFirstDigit lim analyzer)]
       [(list 'Seize peer)
        (send peer 'Seized)
        (send lim 'StartRing)
        (goto RingingBSide lim analyzer peer)]
       [* (goto Idle lim analyzer)]))

   (define-state (GettingFirstDigit lim analyzer) (m)
     (match m
       ['OnHook
        (send lim 'StopTone)
        (goto Idle lim analyzer)]
       [(list 'Digit n)
        (send lim 'StopTone)
        (send analyzer (list 'AnalysisRequest (list 'Cons n 'NoDigits) self))
        (goto WaitOnAnalysis lim analyzer (list 'Cons n 'NoDigits))]
       [(list 'Seize peer)
        (send peer 'Rejected)
        (goto GettingFirstDigit lim analyzer)]
       [* (goto GettingFirstDigit lim analyzer)]))

   (define-state (GettingNumber lim analyzer number) (m)
     (match m
       ['OnHook (goto Idle lim analyzer)]
       [(list 'Digit n)
        (send analyzer (list 'AnalysisRequest (list 'Cons n number) self))
        (goto WaitOnAnalysis lim analyzer (list 'Cons n number))]
       [(list 'Seize peer)
        (send peer 'Rejected)
        (goto GettingNumber lim analyzer number)]
       [* (goto GettingNumber lim analyzer number)]))

   (define-state (WaitOnAnalysis lim analyzer number) (m)
     (match m
       ['OnHook (goto Idle lim analyzer)]
       [(list 'Seize peer)
        (send peer 'Rejected)
        (goto WaitOnAnalysis lim analyzer number)]
       ['Invalid
        (send lim (list 'StartTone 'Fault))
        (goto WaitOnHook lim analyzer 'HaveTone)]
       [(list 'Valid peer)
        (send peer (list 'Seize self))
        (goto MakeCallToB lim analyzer peer)]
       ['GetMoreDigits (goto GettingNumber lim analyzer number)]
       ;; Note: because we don't have selective receive, we throw away any numbers dialed while
       ;; waiting on the analysis. Ideally we would save them in some sort of stack instead
       [* (goto WaitOnAnalysis lim analyzer number)]))

   ;; Called "calling_B" in Ulf's version
   (define-state (MakeCallToB lim analyzer peer) (m)
     (match m
       ['OnHook (goto Idle lim analyzer)]
       [(list 'Seize new-peer)
        (send new-peer 'Rejected)
        (goto MakeCallToB lim analyzer peer)]
       ['Seized
        (send lim (list 'StartTone 'Ring))
        (goto RingingASide lim analyzer peer)]
       ['Rejected
        (send lim (list 'StartTone 'Busy))
        (goto WaitOnHook lim analyzer 'HaveTone)]
       [* (MakeCallToB lim analyzer peer)]))

   ;; the other phone is ringing
   (define-state (RingingASide lim analyzer peer) (m)
     (match m
       [(list 'Seize new-peer)
        (send new-peer 'Rejected)
        (goto RingingASide lim analyzer peer)]
       ['Answered
        (send lim 'StopTone)
        (send lim (list 'Connect peer))
        (goto Speech lim analyzer peer)]
       ['OnHook
        (send peer 'Cleared)
        (send lim 'StopTone)
        (goto Idle lim analyzer)]
       [* (goto RingingASide lim analyzer peer)]))

   ;; this phone is ringing
   (define-state (RingingBSide lim analyzer peer) (m)
     (match m
       [(list 'Seize new-peer)
        (send new-peer 'Rejected)
        (goto RingingBSide lim analyzer peer)]
       ['Cleared
        (send lim 'StopRing)
        (goto Idle lim analyzer)]
       ['OffHook
        (send lim 'StopRing)
        (send peer 'Answered)
        (goto Speech lim analyzer peer)]
       [* (goto RingingBSide lim analyzer peer)]))

   (define-state (Speech lim analyzer peer) (m)
     (match m
       [(list 'Seize new-peer)
        (send new-peer 'Rejected)
        (goto Speech lim analyzer peer)]
       ['Cleared (goto WaitOnHook lim analyzer 'NoTone)]
       ['OnHook
        (send lim (list 'Disconnect peer))
        (send peer 'Cleared)
        (goto Idle lim analyzer)]
       [* (goto Speech lim analyzer peer)]))

   (define-state (WaitOnHook lim analyzer have-tone?) (m)
     (match m
       [(list 'Seize new-peer)
        (send new-peer 'Rejected)
        (goto WaitOnHook lim analyzer have-tone?)]
       ['OnHook
        (match have-tone?
          ['HaveTone
           (send lim 'StopTone)
           (goto Idle lim analyzer)]
          ['NoTone (goto Idle lim analyzer)])]
       [* (goto WaitOnHook lim analyzer have-tone?)]))))

;; ---------------------------------------------------------------------------------------------------
;; Specifications

;; Specification from POV of another phone
(spec
 (goto Idle)

 (define-state (Idle)
   [(list 'Seize peer) ->
    (with-outputs ([peer 'Seized])
      (goto Ringing peer))]
   [(list 'Seize peer) ->
    (with-outputs ([peer 'Rejected])
      (goto Idle))]
   ['Cleared -> (goto Idle)])

 (define-state (Ringing peer)
   ; can answer, can respond to a clear, can respond to seized
   [unobs ->
     (with-outputs ([peer 'Answered])
       (goto InCall peer))]
   [(list 'Seize other-peer) ->
    (with-outputs ([other-peer 'Rejected])
      (goto Ringing peer))]
   ['Cleared -> (goto Idle)])

 (define-state (InCall peer)
   [unobs -> (with-outputs ([peer 'Answered]) (goto InCall peer))]
   [unobs -> (with-outputs ([peer 'Cleared]) (goto InCall peer))]
   [(list 'Seize other-peer) ->
    (with-outputs ([other-peer 'Rejected])
      (goto InCall peer))]
   ['Cleared -> (goto Idle)]))
