#lang csa

;; An implementation of the POTS system often used as an Erlang tutorial, found here:
;; https://github.com/uwiger/pots

;; Difference's from Ulf's version:
;; * all messages other than number analysis are send-and-forget
;; * any response to a request is assumed to be for the most recent request (i.e. no "Ref"s)

;; Notes on the protocol:
;; * regardless of who started the call, the side that hangs up first sends Disconnect to the LIM
;; * "A" side is the calling side, "B" side is the called side

(provide
 PotsAgent

 OffHook
 OnHook
 Digit

 StartRing
 StopRing
 Connect
 Disconnect
 StartTone
 StopTone

 Dial
 Ring
 Busy
 Fault

 Seize
 Seized
 Rejected
 Cleared
 Answered

 GetMoreDigits
 Invalid
 Valid)

;; ---------------------------------------------------------------------------------------------------
;; Types

(define-variant-type Unit [Unit])

(define-variant-type HardwareEvent
  [OffHook]
  [OnHook]
  [Digit [digit Nat]])

;; Assume that the lim somehow has our address attached to these messages. Perhaps we just layer this
;; agent inside another that attaches the address as needed.
(define-variant-type LimCommand
  (StartRing)
  (StopRing)
  (Connect [address (ChannelOf PeerMessage)])
  (Disconnect [address (ChannelOf PeerMessage)])
  (StartTone [tone-type ToneType])
  (StopTone)) ; note: the book didn't list StopTone as a command (p. 184)

(define-variant-type ToneType [Dial] [Ring] [Busy] [Fault])

;; NOTE: names for some of these are different in Ulf's version, and some take an extra Pid parameter
;; in his version
(define-variant-type PeerMessage
  [Seize [peer-channel (ChannelOf PeerMessage)]] ; Ulf's: "request_connection"
  [Seized] ; Ulf's: "accept"
  [Rejected] ; Ulf's "reject"
  [Cleared] ; Ulf's: "cancel"
  ;; note: the book didn't list Answered as one of the possible messages (pp. 184-185)
  [Answered]) ; Ulf's: "connect"

(define-variant-type AnalysisRequest
  [AnalysisRequest [digits DigitList] [response-channel (ChannelOf AnalysisResult)]])

(define-variant-type DigitList
  [NoDigits]
  [Cons [first-digit Nat] [remaining-digits DigitList]])

(define-variant-type AnalysisResult
  [GetMoreDigits]
  [Invalid]
  [Valid [peer-channel (ChannelOf PeerMessage)]])

(define-variant-type HaveTone?
  [HaveTone]
  [NoTone])

;; ---------------------------------------------------------------------------------------------------

(define (PotsAgent lim analyzer)
  (spawn-agent
   (([hw-event HardwareEvent] [from-peer PeerMessage] [analysis-result AnalysisResult])

    (define-state (Idle [lim (ChannelOf LimCommand)] [analyzer (ChannelOf AnalysisRequest)])
      [hw-event (e)
        (case e
          [OffHook ()
            (send lim (StartTone (Dial)))
            (goto GettingFirstDigit lim analyzer)]
          [OnHook () (goto Idle lim analyzer)]
          [Digit (d) (goto Idle lim analyzer)])]
      [from-peer (m)
        (case m
          [Seize (peer-chan)
            (send peer-chan (Seized))
            (send lim (StartRing))
            (goto RingingBSide lim analyzer peer-chan)]
          [Seized () (goto Idle lim analyzer)]
          [Rejected () (goto Idle lim analyzer)]
          [Cleared () (goto Idle lim analyzer)]
          [Answered () (goto Idle lim analyzer)])]
      [analysis-result (r) (goto Idle lim analyzer)])

    (define-state (GettingFirstDigit [lim (ChannelOf LimCommand)]
                                     [analyzer (ChannelOf AnalysisRequest)])
      [hw-event (e)
        (case e
          [OffHook () (goto GettingFirstDigit lim analyzer)]
          [OnHook ()
            (send lim (StopTone))
            (goto Idle lim analyzer)]
          [Digit (n)
            (send lim (StopTone))
            (send analyzer (AnalysisRequest (Cons n (NoDigits)) analysis-result))
            (goto WaitOnAnalysis lim analyzer (Cons n (NoDigits)))])]
      [from-peer (m)
        (case m
          [Seize (peer-chan)
            (send peer-chan (Rejected))
            (goto GettingFirstDigit lim analyzer)]
          [Seized () (goto GettingFirstDigit lim analyzer)]
          [Rejected () (goto GettingFirstDigit lim analyzer)]
          [Cleared () (goto GettingFirstDigit lim analyzer)]
          [Answered () (goto GettingFirstDigit lim analyzer)])]
      [analysis-result (r) (goto GettingFirstDigit lim analyzer)])

    (define-state (GettingNumber [lim (ChannelOf LimCommand)]
                                 [analyzer (ChannelOf AnalysisRequest)]
                                 [number DigitList])
      [hw-event (e)
        (case e
          [OffHook () (goto GettingNumber lim analyzer)]
          [OnHook () (goto Idle lim analyzer)]
          [Digit (n)
            (send analyzer (AnalysisRequest (Cons n number) analysis-result))
            (goto WaitOnAnalysis lim analyzer (Cons n number))])]
      [from-peer (m)
        (case m
          [Seize (peer-chan)
            (send peer-chan (Rejected))
            (goto GettingNumber lim analyzer number)]
          [Seized () (goto GettingNumber lim analyzer number)]
          [Rejected () (goto GettingNumber lim analyzer number)]
          [Cleared () (goto GettingNumber lim analyzer number)]
          [Answered () (goto GettingNumber lim analyzer number)])]
      [analysis-result (r) (goto GettingNumber lim analyzer number)])

    (define-state (WaitOnAnalysis [lim (ChannelOf LimCommand)]
                                  [analyzer (ChannelOf AnalysisRequest)]
                                  [number DigitList])
      [hw-event (e)
        (case e
          [OffHook () (goto WaitOnAnalysis lim analyzer number)]
          [OnHook () (goto Idle lim analyzer)]
          ;; Note: because we don't have selective receive, we throw away any numbers dialed while
          ;; waiting on the analysis. Ideally we would save them in some sort of stack instead
          [Digit (n) (goto WaitOnAnalysis lim analyzer number)])]
      [from-peer (m)
        (case m
          [Seize (peer-chan)
            (send peer-chan (Rejected))
            (goto WaitOnAnalysis lim analyzer number)]
          [Seized () (goto WaitOnAnalysis lim analyzer number)]
          [Rejected () (goto WaitOnAnalysis lim analyzer number)]
          [Cleared () (goto WaitOnAnalysis lim analyzer number)]
          [Answered () (goto WaitOnAnalysis lim analyzer number)])]
      [analysis-result (r)
        (case r
          [Invalid ()
            (send lim (StartTone (Fault)))
            (goto WaitOnHook lim analyzer (HaveTone))]
          [Valid (peer-chan)
            (send peer-chan (Seize from-peer))
            (goto MakeCallToB lim analyzer peer-chan)]
          [GetMoreDigits () (goto GettingNumber lim analyzer number)])])

    ;; Called "calling_B" in Ulf's version
    (define-state (MakeCallToB [lim (ChannelOf LimCommand)]
                               [analyzer (ChannelOf AnalysisRequest)]
                               [peer (ChannelOf PeerMessage)])
      [hw-event (e)
        (case e
          [OffHook () (goto MakeCallToB lim analyzer peer)]
          [OnHook () (goto Idle lim analyzer)]
          [Digit (d) (goto MakeCallToB lim analyzer peer)])]
      [from-peer (m)
        (case m
          [Seize (new-peer)
            (send new-peer (Rejected))
            (goto MakeCallToB lim analyzer peer)]
          [Seized ()
            (send lim (StartTone (Ring)))
            (goto RingingASide lim analyzer peer)]
          [Rejected ()
            (send lim (StartTone (Busy)))
            (goto WaitOnHook lim analyzer (HaveTone))]
          [Cleared () (goto MakeCallToB lim analyzer peer)]
          [Answered () (MakeCallToB lim analyzer peer)])]
      [analysis-result (r) (goto GettingFirstDigit lim analyzer)])

    (define-state (RingingASide [lim (ChannelOf LimCommand)]
                                [analyzer (ChannelOf AnalysisRequest)]
                                [peer (ChannelOf PeerMessage)])
      [from-peer (m)
        (case m
          [Seize (new-peer)
            (send new-peer (Rejected))
            (goto RingingASide lim analyzer peer)]
          [Seized () (goto RingingASide lim analyzer peer)]
          [Rejected () (goto RingingASide lim analyzer peer)]
          [Cleared () (goto RingingASide lim analyzer peer)]
          [Answered ()
            (send lim (StopTone))
            (send lim (Connect peer))
            (goto Speech lim analyzer peer)])]
      [hw-event (e)
        (case e
          [OffHook () (goto RingingASide lim analyzer peer)]
          [OnHook ()
            (send peer (Cleared))
            (send lim (StopTone))
            (goto Idle lim analyzer)]
          [Digit () (goto RingingASide lim analyzer peer)])]
      [analysis-result (r) (goto RingingASide lim analyzer peer)])

    ;; this phone is ringing
    (define-state (RingingBSide [lim (ChannelOf LimCommand)]
                                [analyzer (ChannelOf AnalysisRequest)]
                                [peer (ChannelOf PeerMessage)])
      [from-peer (m)
        (case m
          [Seize (new-peer)
            (send new-peer (Rejected))
            (goto RingingBSide lim analyzer peer)]
          [Cleared ()
            (send lim StopRing)
            (goto Idle lim analyzer)]
          [Seized () (goto RingingBSide lim analyzer peer)]
          [Rejected () (goto RingingBSide lim analyzer peer)]
          [Answered () (goto RingingBSide lim analyzer peer)])]
      [hw-event (e)
        (case e
          [OffHook ()
            (send lim (StopRing))
            (send peer (Answered))
            (goto Speech lim analyzer peer)]
          [OnHook () (goto RingingBSide lim analyzer peer)]
          [Digit (d) (goto RingingBSide lim analyzer peer)])]
      [analysis-result (r) (goto RingingBSide lim analyzer peer)])

    (define-state (Speech [lim (ChannelOf LimCommand)]
                          [analyzer (ChannelOf AnalysisRequest)]
                          [peer (ChannelOf PeerMessage)])
      [from-peer (m)
        (case m
          [Seize (new-peer)
            (send new-peer (Rejected))
            (goto Speech lim analyzer peer)]
          [Seized () (goto Speech lim analyzer peer)]
          [Rejected () (goto Speech lim analyzer peer)]
          [Cleared () (goto WaitOnHook lim analyzer (NoTone))]
          [Answered () (goto Speech lim analyzer peer)])]
      [hw-event (e)
        (case e
          [OffHook () (goto Speech lim analyzer peer)]
          [OnHook ()
            (send lim (Disconnect peer))
            (send peer (Cleared))
            (goto Idle lim analyzer)]
          [Digit (d) (goto Speech lim analyzer peer)])]
      [analysis-result (r) (goto Speech lim analyzer peer)])

    (define-state (WaitOnHook [lim (ChannelOf LimCommand)]
                              [analyzer (ChannelOf AnalysisRequest)]
                              [have-tone? HaveTone])
      [hw-event (e)
        (case e
          [OffHook () (goto WaitOnHook lim analyzer have-tone?)]
          [OnHook ()
            (case have-tone?
             [HaveTone ()
               (send lim (StopTone))
               (goto Idle lim analyzer)]
             [NoTone () (goto Idle lim analyzer)])]
          [Digit (d) (goto WaitOnHook lim analyzer have-tone?)])]
      [from-peer (m)
        (case m
          [Seize (new-peer)
            (send new-peer (Rejected))
            (goto WaitOnHook lim analyzer have-tone?)]
          [Seized ()   (goto WaitOnHook lim analyzer have-tone?)]
          [Rejected () (goto WaitOnHook lim analyzer have-tone?)]
          [Cleared ()  (goto WaitOnHook lim analyzer have-tone?)]
          [Answered () (goto WaitOnHook lim analyzer have-tone?)])]
      [analysis-result (r) (goto WaitOnHook lim analyzer have-tone?)])

    (goto Idle lim analyzer))

   (list hw-event from-peer analysis-result)))
