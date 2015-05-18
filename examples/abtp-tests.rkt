#lang racket

(require "abtp.rkt")
(require rackunit)
(require asyncunit)
(require racket/async-channel)

;; ---------------------------------------------------------------------------------------------------
;; Sender tests

(define (make-sender)
  (define receiver-channel (make-async-channel))
  (define status-channel (make-async-channel))
  (match-define (list message-to-send close-session from-receiver)
    (Sender receiver-channel 80 status-channel))
  (values message-to-send
          close-session
          from-receiver
          receiver-channel
          status-channel))

(define (goto-Connected from-receiver status-channel)
  (async-channel-put from-receiver 'SynAck)
  (check-unicast-match status-channel (list 'Connected _ _)))

(test-case
 "Initial connection time-out"
 (match-define-values (_ _ _ _ status-channel) (make-sender))
 (sleep 3)
 (check-unicast status-channel 'ConnectFailed))

(test-case
 "Initial connection success"

 (match-define-values (_ _ from-receiver _ status-channel) (make-sender))
 (async-channel-put from-receiver 'SynAck)
 (check-unicast-match status-channel (list 'Connected _ _))
 ;; void here just prevents output when running the test
 (void))

(test-case
 "Timeout on message send, with multiples sent"
 (match-define-values (message-to-send _ from-receiver to-receiver status-channel)
   (make-sender))
 (goto-Connected from-receiver status-channel)
 (define req-response (make-async-channel))
 (async-channel-put message-to-send req-response)
 (check-unicast req-response 'Queued)
 (sleep 9)
 (check-unicast status-channel 'ErrorClosed)
 (check-unicast-match to-receiver _) ; ignore the Syn
 (check-unicast-match to-receiver (list 'Write _ 'Seq0 'Unit))
 (check-unicast-match to-receiver (list 'Write _ 'Seq0 'Unit))
 (check-unicast-match to-receiver (list 'Write _ 'Seq0 'Unit)))

;; ---------------------------------------------------------------------------------------------------
;; Receiver tests

(test-case
 "New receiver broadcasts an Ack1 periodically"
 (define to-net (make-async-channel))
 (define to-app (make-async-channel))
 (match-define  (list _) (Receiver to-net to-app))
 (check-unicast to-net 'SynAck)
 (check-unicast to-net 'Ack1 #:timeout 1.1)
 (check-unicast to-net 'Ack1 #:timeout 1.1))

(test-case
 "Sending Seq0 results in an Ack0, eventually"
 (define to-net (make-async-channel))
 (define to-app (make-async-channel))
 (match-define (list from-sender) (Receiver to-net to-app))
 (async-channel-put from-sender (list 'Write 80 'Seq0 'Unit))
 (define first-response (check-unicast-match to-net response #:result response))
 (unless (equal? first-response 'Ack0)
   (check-unicast to-net 'Ack0)))

(test-case
 "Sending Seq1 doesn't change the Ack"
 (define to-net (make-async-channel))
 (define to-app (make-async-channel))
 (match-define (list from-sender) (Receiver to-net to-app))
 (check-unicast to-net 'SynAck)
 (async-channel-put from-sender (list 'Write 80 'Seq1 'Unit))
 (check-unicast to-net 'Ack1 #:timeout 1.1)
 (check-unicast to-net 'Ack1 #:timeout 1.1))

;; ---------------------------------------------------------------------------------------------------
;; Manager tests

;; Check that we can request a new session, send a message, get a "queued" back, send "close", get
;; "closing", "closed" back

(test-case
 "Conversation through ABTP service"
 (define app-channel (make-async-channel))
 (define send-response-channel (make-async-channel))
 (define connection-status-channel (make-async-channel))
 (match-define (list start-sender from-net)
   (Manager app-channel (make-async-channel)))
 (async-channel-put start-sender
                    (list 'Connect from-net 80 connection-status-channel))
 (match-define (list send-channel close-channel)
   (check-unicast-match connection-status-channel
                        (list 'Connected send-channel close-channel)
                        #:result (list send-channel close-channel)))
 (async-channel-put send-channel send-response-channel)
 (check-unicast send-response-channel 'Queued)
 (sleep .1)
 (async-channel-put close-channel 'Unit)
 (check-unicast connection-status-channel 'Closed)
 (async-channel-put send-channel send-response-channel)
 (check-unicast send-response-channel 'WriteFailed))
