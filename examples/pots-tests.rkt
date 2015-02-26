#lang racket

;; Tests for the POTS implementation

;; ---------------------------------------------------------------------------------------------------

(require "pots.rkt"
         rackunit
         asyncunit
         racket/async-channel)

(module+ main
  (test-case
   "Send seized and ring after an initial 'seize'"
   (define hw-channel (make-async-channel))
   (match-define (list hw-event from-peer analysis-result)
     (PotsAgent hw-channel (make-async-channel)))
   (define peer (make-async-channel))
   (async-channel-put from-peer (Seize peer))
   (check-unicast peer (Seized))
   (check-unicast hw-channel (StartRing)))

  (test-case
   "Off-hook causes the dial tone to start"
   (define hw-channel (make-async-channel))
   (match-define (list hw-event from-peer analysis-result)
     (PotsAgent hw-channel (make-async-channel)))
   (async-channel-put hw-event (OffHook))
   (check-unicast hw-channel (StartTone (Dial))))

  (test-case
   "Getting a digit causes a call to the analyzer"
   (define hw-channel (make-async-channel))
   (define analysis-channel (make-async-channel))
   (match-define (list hw-event from-peer analysis-result) (PotsAgent hw-channel analysis-channel))
   (async-channel-put hw-event (OffHook))
   (check-unicast-match hw-channel _) ; consume the dial tone message
   (async-channel-put hw-event (Digit 2))
   (check-unicast-match analysis-channel (list 'AnalysisRequest (list 'Cons 2 _) _)))

  (test-case
   "An invalid number starts an error tone"
   (define hw-channel (make-async-channel))
   (define analysis-channel (make-async-channel))
   (match-define (list hw-event from-peer analysis-result) (PotsAgent hw-channel analysis-channel))
   (async-channel-put hw-event (OffHook))
   (check-unicast-match hw-channel _) ; consume the dial tone message
   (async-channel-put hw-event (Digit 2))
   (check-unicast hw-channel (StopTone)) ; consume the stop-tone message
   (define result-channel
     (check-unicast-match analysis-channel (list 'AnalysisRequest (list 'Cons 2 _) r) #:result r))
   (async-channel-put result-channel (Invalid))
   (check-unicast hw-channel (StartTone (Fault))))

  (test-case
   "More digits after invalid number does not stop error tone"
   (define hw-channel (make-async-channel))
   (define analysis-channel (make-async-channel))
   (match-define (list hw-event from-peer analysis-result) (PotsAgent hw-channel analysis-channel))
   (async-channel-put hw-event (OffHook))
   (check-unicast-match hw-channel _) ; consume the dial tone message
   (async-channel-put hw-event (Digit 2))
   (check-unicast hw-channel (StopTone)) ; consume the stop-tone message
   (define result-channel
     (check-unicast-match analysis-channel (list 'AnalysisRequest (list 'Cons 2 _) r) #:result r))
   (async-channel-put result-channel (Invalid))
   (check-unicast-match hw-channel _) ; consume the start tone message
   (async-channel-put hw-event (Digit 2))
   (check-no-message hw-channel #:timeout 1))

  (test-case
   "Hanging up after invalid number stops the error tone"
   (define hw-channel (make-async-channel))
   (define analysis-channel (make-async-channel))
   (match-define (list hw-event from-peer analysis-result) (PotsAgent hw-channel analysis-channel))
   (async-channel-put hw-event (OffHook))
   (check-unicast-match hw-channel _) ; consume the dial tone message
   (async-channel-put hw-event (Digit 2))
   (check-unicast hw-channel (StopTone)) ; consume the stop-tone message
   (define result-channel
     (check-unicast-match analysis-channel (list 'AnalysisRequest (list 'Cons 2 _) r) #:result r))
   (async-channel-put result-channel (Invalid))
   (check-unicast-match hw-channel _) ; consume the start tone message
   (async-channel-put hw-event (OnHook))
   (check-unicast hw-channel (StopTone)))

  (test-case
   "A full number causes a Seize message to the peer"
   (define hw-channel (make-async-channel))
   (define analysis-channel (make-async-channel))
   (match-define (list hw-event from-peer analysis-result) (PotsAgent hw-channel analysis-channel))
   (Idle->GettingNumber hw-event hw-channel)
   (async-channel-put hw-event (Digit 2))
   (define result-chan
     (check-unicast-match analysis-channel
                          (list 'AnalysisRequest (list 'Cons 2 (list 'NoDigits)) r)
                          #:result r))
   (async-channel-put result-chan (GetMoreDigits))
   (sleep 0.1)
   (async-channel-put hw-event (Digit 3))
   (define result-chan2
     (check-unicast-match analysis-channel
                          (list 'AnalysisRequest (list 'Cons 3 (list 'Cons 2 (list 'NoDigits))) r)
                          #:result r))

   (define peer-chan (make-async-channel))
   (async-channel-put result-chan2 (Valid peer-chan))
   (check-unicast-match peer-chan (list 'Seize _))) ; TODO: fix this one

  (test-case
   "Getting a Seized response causes a Ring tone"
   (define hw-channel (make-async-channel))
   (define analysis-channel (make-async-channel))
   (match-define (list hw-event from-peer analysis-result) (PotsAgent hw-channel analysis-channel))
   (Idle->GettingNumber hw-event hw-channel)
   (GettingNumber->MakeCallToB hw-event hw-channel analysis-channel)
   (async-channel-put from-peer (Seized))
   (check-unicast hw-channel (StartTone (Ring))))

  (test-case
   "If B side answers, the ringing tone stops and the calls connect"
   (define hw-channel (make-async-channel))
   (define analysis-channel (make-async-channel))
   (match-define (list hw-event from-peer analysis-result) (PotsAgent hw-channel analysis-channel))
   (Idle->GettingNumber hw-event hw-channel)
   (define peer-chan (GettingNumber->MakeCallToB hw-event hw-channel analysis-channel))
   (MakeCallToB->RingingASide from-peer hw-channel)
   (async-channel-put from-peer (Answered))
   (check-unicast hw-channel (StopTone))
   (check-unicast-match hw-channel (list 'Connect (== peer-chan))))

  (test-case
   "During Speech started by self, if self hangs up, self sends Disconnect"
   (define hw-channel (make-async-channel))
   (define analysis-channel (make-async-channel))
   (match-define (list hw-event from-peer analysis-result) (PotsAgent hw-channel analysis-channel))
   (Idle->GettingNumber hw-event hw-channel)
   (define peer-chan (GettingNumber->MakeCallToB hw-event hw-channel analysis-channel))
   (MakeCallToB->RingingASide from-peer hw-channel)
   (RingingASide->Speech from-peer hw-channel)
   (async-channel-put hw-event (OnHook))
   (check-unicast-match hw-channel (list 'Disconnect (== peer-chan))))

  (test-case
   "During Speech started by self, if peer hangs up, self does not send Disconnect"
   (define hw-channel (make-async-channel))
   (define analysis-channel (make-async-channel))
   (match-define (list hw-event from-peer analysis-result) (PotsAgent hw-channel analysis-channel))
   (Idle->GettingNumber hw-event hw-channel)
   (define peer-chan (GettingNumber->MakeCallToB hw-event hw-channel analysis-channel))
   (MakeCallToB->RingingASide from-peer hw-channel)
   (RingingASide->Speech from-peer hw-channel)
   (async-channel-put from-peer (Cleared))
   (check-no-message hw-channel #:timeout 1))

  (test-case
   "During Speech started by peer, if self hangs up, then self disconnects"
   (define hw-channel (make-async-channel))
   (define analysis-channel (make-async-channel))
   (match-define (list hw-event from-peer analysis-result) (PotsAgent hw-channel analysis-channel))
   (define peer-channel (make-async-channel))
   (Idle->RingingBSide from-peer hw-channel peer-channel)
   (RingingBSide->Speech hw-event hw-channel peer-channel)
   (async-channel-put hw-event (OnHook))
   (check-unicast-match hw-channel (list 'Disconnect (== peer-channel))))

  (test-case
   "During Speech started by peer, if peer hangs up, we do NOT disconnect"
   (define hw-channel (make-async-channel))
   (define analysis-channel (make-async-channel))
   (match-define (list hw-event from-peer analysis-result) (PotsAgent hw-channel analysis-channel))
   (define peer-channel (make-async-channel))
   (Idle->RingingBSide from-peer hw-channel peer-channel)
   (RingingBSide->Speech hw-event hw-channel peer-channel)
   (async-channel-put from-peer (Cleared))
   (check-no-message hw-channel #:timeout 1))

  (test-case
   "During WaitOnHook where we have no tone, the OnHook event causes no observable event"
   (define hw-channel (make-async-channel))
   (define analysis-channel (make-async-channel))
   (match-define (list hw-event from-peer analysis-result) (PotsAgent hw-channel analysis-channel))
   (define peer-channel (make-async-channel))
   (Idle->RingingBSide from-peer hw-channel peer-channel)
   (RingingBSide->Speech hw-event hw-channel peer-channel)
   (Speech->WaitOnHook from-peer hw-channel)
   (async-channel-put hw-event (OnHook))
   (check-no-message hw-channel)))

;; ---------------------------------------------------------------------------------------------------
;; Transition functions

(define (Idle->GettingNumber hw-event hw-channel)
  (async-channel-put hw-event (OffHook))
  (check-unicast-match hw-channel _)) ; consume the dial tone message

(define (GettingNumber->MakeCallToB hw-event hw-channel analysis-channel)
  (async-channel-put hw-event (Digit 1))
  (check-unicast hw-channel (StopTone)) ; consume the stop-tone message
  (define result-chan
    (check-unicast-match analysis-channel
                         (list 'AnalysisRequest (list 'Cons 1 (list 'NoDigits)) r)
                         #:result r))
  (define peer-chan (make-async-channel))
  (async-channel-put result-chan (Valid peer-chan))
  (check-unicast-match peer-chan (list 'Seize _))
  peer-chan)

(define (MakeCallToB->RingingASide from-peer hw-channel)
  (async-channel-put from-peer (Seized))
  (check-unicast hw-channel (StartTone (Ring))))

(define (RingingASide->Speech from-peer hw-channel)
  (async-channel-put from-peer (Answered))
  (check-unicast-match hw-channel _)
  (check-unicast-match hw-channel _))

(define (Idle->RingingBSide from-peer hw-channel peer-chan)
  (async-channel-put from-peer (Seize peer-chan))
  (check-unicast-match hw-channel _)) ; consume the StartRing

(define (RingingBSide->Speech hw-event hw-channel peer-channel)
  (async-channel-put hw-event (OffHook))
  (check-unicast-match hw-channel _) ; consume the StopRing
  (check-unicast-match peer-channel _)) ; consume the Answered

(define (Speech->WaitOnHook from-peer hw-channel)
  (async-channel-put from-peer (Cleared))
  (sync/timeout 0.5 hw-channel)) ; consume a possible Disconnect event
