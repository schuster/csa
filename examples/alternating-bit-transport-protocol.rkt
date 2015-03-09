;; TODO: put purpose statements on components and states

#lang csa

;; Implements actors and specifications for a variant of the alternating bit protocol, with a two-way
;; handshake for both the beginning and end of a session.

(provide Sender
         Receiver
         Manager

         Connected
         ConnectFailed
         Closed
         ErrorClosed

         Syn
         Message
         Fin
         Seq0
         Seq1

         SynAck
         Ack0
         Ack1
         FinAck

         Unit

         Queued
         WriteFailed

         Connect)

;; ---------------------------------------------------------------------------------------------------

;; Wire-level messages
(define-variant-type SenderMessage
  [Syn Nat (Channel ReceiverMessage)]
  [Message Nat SequenceNumber Unit]
  [Fin Nat])
(define-variant-type ReceiverMessage [SynAck] [Ack0] [Ack1] [FinAck])

(define-variant-type SequenceNumber [Seq0] [Seq1])

(define-variant-type AttemptCount
  [OneAttempt]
  [TwoAttempts]
  [ThreeAttempts])

;; Client-level messages
(define-variant-type WriteResponse (Queued) (WriteFailed))
(define-variant-type Status
  [Connected (Channel Unit) (Channel Unit)]
  [ConnectFailed]
  [Closed]
  [ErrorClosed])

(define-variant-type AppMessage
  [AppMessage Nat ; port
              Unit]) ; content

(define-variant-type Connect
   (Connect (Channel ReceiverMessage)
            Nat ; port
            (Channel Status)))

;; Misc. types
(define-variant-type Unit [Unit])

(define-variant-type EntryList
  [Null]
  [Cons Entry EntryList])
(define-variant-type Entry
  [Entry Natural ; port
         (Channel SenderMessage)])

(define-variant-type MessageStack
  [EmptyStack]
  [Stack MessageStack]) ; note: we currently omit the message for simplicity

;; ---------------------------------------------------------------------------------------------------
;; Actors

(define (Receiver to-sender to-app)
  (spawn-agent
   (([from-sender SenderMessage])
    (define-state (Respond0 [to-sender (ChannelOf ReceiverMessage)]
                            ;; should be AppMessage
                            [to-app (ChannelOf AppMessage)])
      [from-sender (msg)
        (case msg
          [Syn (p) (goto Respond0 to-sender to-app)] ; ignore Syn
          [Message (port seq content)
           (case seq
             [Seq0 () (goto Respond0 to-sender to-app)]
             [Seq1 ()
               (send to-app (AppMessage port content))
               (send to-sender (Ack1))
               (goto Respond1 to-sender to-app)])]
          [Fin (p)
            (send to-sender (FinAck))
            (goto Closed)])]
      [(timeout 1)
       (send to-sender (Ack0))
       (goto Respond0 to-sender to-app)])

    (define-state (Respond1 [to-sender (ChannelOf ReceiverMessage)]
                            [to-app (ChannelOf Unit)])
      [from-sender (msg)
        (case msg
          [Syn (p) (goto Respond1 to-sender to-app)] ; ignore Syn
          [Message (port seq content)
           (case seq
             [Seq0 ()
               (send to-app (AppMessage port content))
               (send to-sender (Ack0))
               (goto Respond0 to-sender to-app)]
             [Seq1 () (goto Respond1 to-sender to-app)])]
          [Fin (p)
            (send to-sender (FinAck))
            (goto Closed)])]
      [(timeout 1)
       (send to-sender (Ack1))
       (goto Respond1 to-sender to-app)])

    (define-state (Closed)
      [from-sender (msg) (goto Closed)])

    (begin
      (send to-sender (SynAck))
      (goto Respond1 to-sender to-app)))

   (list from-sender)))

(define (Sender to-recvr receiver-port status)
  (spawn-agent
   (([write Unit] [close Unit] [from-recvr ReceiverMessage])

    (define-state
      (SynSent [to-recvr (ChannelOf SenderMessage)]
               [port Nat]
               [status (ChannelOf Status)])
      ;; NOTE: we use goto-this-state as an obvious shorthand
      [write (m) (goto-this-state)]
      [close (m) (goto-this-state)]
      [from-recvr (m)
        (case m
          [SynAck ()
            (send status (Connected write close))
            (goto Ready (Seq0) to-recvr port status)]
          ;; NOTE: we use goto-this-state as an obvious shorthand
          [Ack1 () (goto-this-state)]
          [Ack0 () (goto-this-state)]
          [FinAck () (goto-this-state)])]
      [(timeout 3)
        (send status (ConnectFailed))
        (goto ClosedState)])

    (define-state (Ready [current-seq SequenceNumber]
                         [to-recvr (ChannelOf SenderMessage)]
                         [port Nat]
                         [status (ChannelOf Status)])
      [write (response)
        (send to-recvr (Message port current-seq (Unit)))
        (send response (Queued))
        (goto AwaitingAck current-seq (OneAttempt) (EmptyStack) (EmptyStack) to-recvr port status)]
      [close (c)
        (send to-recvr (Fin port))
        (goto Closing status)]
      [from-recvr (m)
        ;; can ignore all receiver messages in this state
        (goto Ready current-seq to-recvr port status)])

    (define-state (AwaitingAck [last-sent-seq SequenceNumber]
                               [send-attempts AttemptCount]
                               ;; these two stacks implement a queue in the purely functional style
                               [enqueue-stack MessageStack]
                               [dequeue-stack MessageStack]
                               [to-recvr (ChannelOf SenderMessage)]
                               [port Nat]
                               [status (ChannelOf Status)])
      [write (r)
        (send r (Queued))
        (goto AwaitingAck last-sent-seq send-attempts (Stack enqueue-stack) dequeue-stack to-recvr port status)]
      [close (c)
        (send to-recvr (Fin port))
        (goto Closing status)]
      [from-recvr (m)
        (case m
          [SynAck () (goto-this-state)]
          [Ack1 ()
            (case last-sent-seq
              [Seq0 () (goto-this-state)]
              [Seq1 ()
                ;; check for any queued messages to send
                (case dequeue-stack
                  [EmptyStack () (goto Rebalancing (Seq0) enqueue-stack dequeue-stack to-recvr port status)]
                  [Stack (dequeue-stack)
                    (send to-recvr (Message port (Seq0) (Unit)))
                    (goto AwaitingAck (Seq0) (OneAttempt) enqueue-stack dequeue-stack to-recvr port status)])])]
          [Ack0 ()
            (case last-sent-seq
              [Seq0 ()
                (case dequeue-stack
                  [EmptyStack () (goto Rebalancing (Seq1) enqueue-stack dequeue-stack to-recvr port status)]
                  [Stack (dequeue-stack)
                    (send to-recvr (Message port (Seq1) (Unit)))
                    (goto AwaitingAck (Seq1) (OneAttempt) enqueue-stack dequeue-stack to-recvr port status)])]
              [Seq1 () (goto-this-state)])]
          [FinAck () (goto-this-state)])]
      [(timeout 3)
       (case send-attempts
         [OneAttempt ()
           (send to-recvr (Message port last-sent-seq (Unit)))
           (goto AwaitingAck last-sent-seq (TwoAttempts) enqueue-stack dequeue-stack to-recvr port status)]
         [TwoAttempts ()
           (send to-recvr (Message port last-sent-seq (Unit)))
           (goto AwaitingAck last-sent-seq (ThreeAttempts) enqueue-stack dequeue-stack to-recvr port status)]
         [ThreeAttempts ()
          (send status (ErrorClosed))
          (goto ClosedState)])])

    ;; TODO: maybe rename this to "Rebalancing"
    (define-state (Rebalancing [current-seq SequenceNumber]
                              [enqueue-stack MessageStack]
                              [dequeue-stack MessageStack]
                              [to-recvr (ChannelOf SenderMessage)]
                              [port Nat]
                              [status (ChannelOf Status)])
      [(timeout 0)
        (case enqueue-stack
          [EmptyStack ()
            (case dequeue-stack
              [EmptyStack () (goto Ready current-seq to-recvr port status)]
              [Stack (dequeue-stack)
                (send to-recvr (Message port current-seq (Unit)))
                (goto AwaitingAck current-seq (OneAttempt) enqueue-stack dequeue-stack to-recvr port status)])]
          [Stack (enqueue-stack)
            (goto Rebalancing current-seq enqueue-stack (Stack dequeue-stack) to-recvr port status)])]
      [write (r)
        (send r (Queued))
        ;; NOTE: we enqueue the message into the middle of the stack, because we don't have a better
        ;; choice. The lack of recursion means we can't rebalance the entire queue all at once without
        ;; listening to other channels, and because we have no selective receive, we have to listen to
        ;; this channel.
        (goto Rebalancing current-seq enqueue-stack (Stack dequeue-stack) to-recvr port status)]
      [close (c)
        (send to-recvr (Fin port))
        (goto Closing status)]
      [from-recvr (m)
        ;; can ignore all receiver messages in this state
        (goto-this-state)])

    (define-state (Closing [status (ChannelOf Status)])
      [write (r)
        (send r (WriteFailed))
        (goto Closing status)]
      [close (c)
        ;; just ignore this
        (goto-this-state)]
      [from-recvr (m)
        (case m
          [SynAck () (goto-this-state)]
          [Ack0 () (goto-this-state)]
          [Ack1 () (goto-this-state)]
          [FinAck ()
            (send status (Closed))
            (goto ClosedState)])]
      [(timeout 3)
        (send status (ErrorClosed))
        (goto ClosedState)])

    (define-state (ClosedState)
      [write (r)
        (send r (WriteFailed))
        (goto-this-state)]
      [close (c)
        (goto-this-state)]
      [from-recvr (m) (goto-this-state)])

    (begin
      (send to-recvr (Syn receiver-port from-recvr))
      (goto SynSent to-recvr receiver-port status)))
   (list write close from-recvr)))

(define (Manager to-app nic-registration)
  (spawn-agent
   (([connect Connect] [from-net SenderMessage])

    (define-state (Ready [to-app (ChannelOf AppMessage)]
                         [receivers EntryList])
      [connect (req)
        (case req
          [Connect (to-recvr port status)
            ;; We use spawn-named-agent as a small hack to allow separate definition of agents; this
            ;; is effectively equivalent to copy-pasting the the Sender code here into a spawn-agent
            ;; form
            (spawn-named-agent (write close from-recvr)
              (Sender to-recvr port status)
              (goto-this-state))])]
      [from-net (m)
        ;; We assume that any Syn message does not yet have a corresponding Receiver started
        (case m
          [Syn (port to-sender)
            (spawn-named-agent (from-sender) (Receiver to-sender to-app)
              (goto Ready to-app (Cons (Entry port from-sender) receivers)))]
          [Message (port seq content)
            (goto LookingForReceiver m port receivers to-app receivers)]
          [Fin (port)
            (goto LookingForReceiver m port receivers to-app receivers)])])

    (define-state (LookingForReceiver [message SenderMessage]
                                      [port Nat]
                                      [remaining EntryList]
                                      [to-app (ChannelOf AppMessage)]
                                      [receivers EntryList])
      ;; NOTE: we just throw away message in this state, because of the lack of selective
      ;; receive. There are more better, more sophisticated ways to handle this
      [connect (m) (goto-this-state)]
      [from-net (m) (goto-this-state)]
      [(timeout 0)
        (case remaining
          [Null () (goto Ready to-app receivers)]
          [Cons (entry rest)
            (case entry
              [Entry (other-port chan)
                (case (= other-port port)
                 [True ()
                  (send chan message)
                  (goto Ready to-app receivers)]
                 [False () (goto LookingForReceiver message rest to-app receivers)])])])])

    (begin
      (send nic-registration from-net)
      (goto Ready to-app (Null))))

   (list connect from-net)))

;; ---------------------------------------------------------------------------------------------------
;; Specifications

;; Defines the behavior of the ABTP manager, from the consumer's point of view
(define-spec ManagerSpec
  (channels [connect Connect])
  (define-state (Always)
    [connect (Connect * * status) ->
      (Always)
      (activ [s SenderSpec (Connecting status)])]))

;; Defines the behavior of an ABTP sender, from the point of view of the application layer
(define-spec SenderSpec
  (channels [write (ChannelOf WriteResponse)]
            [close Unit])
  (define-state (Connecting [status Status])
    [unobs ->
      (Closed) (out [status (ConnectFailed)])]
    [unobs ->
      (Connected status)
      (out
       [status
        (Connected (spec-chan self write)
                   (spec-chan self close))])])
  (define-state (Connected [status Status])
    [unobs -> (Closed) (out [status (ErrorClosed)])]
    [write r -> (Connected status) (out [r (Queued)])]
    [close * -> (Closing status)])
  (define-state (Closing [status Status])
    [write r -> (Closing status) (out [r (WriteFailed)])]
    [close * -> (Closing status)]
    [unobs -> (Closed) (out [status (ErrorClosed)])]
    [unobs -> (Closed) (out [status (Closed)])])
  (define-state (Closed)
    [write r -> (Closed) (out [r (WriteFailed)])]
    [close * -> (Closed)]))
