#lang csa

;; Implements actors and specifications for a variant of the alternating bit protocol, with a two-way
;; handshake for both the beginning and end of a session.

(provide Sender
         Receiver
         Manager)

;; ---------------------------------------------------------------------------------------------------
;; Actors

;; The receiving side of an ABTP session
(define (Receiver to-sender to-app)
  (spawn-agent
   ((from-sender)

    (begin
      (send to-sender 'SynAck)
      (goto Respond1 to-sender to-app))

    ;; Waiting for a Seq0 message
    (define-state (Respond0 to-sender to-app)
      [from-sender (msg)
        (match msg
          [(list 'Syn p) (goto Respond0 to-sender to-app)] ; ignore Syn
          [(list 'Write port seq content)
           (match seq
             ['Seq0 (goto Respond0 to-sender to-app)]
             ['Seq1
               (send to-app (list 'AppMessage port content))
               (send to-sender 'Ack1)
               (goto Respond1 to-sender to-app)])]
          [(list 'Fin p)
            (send to-sender 'FinAck)
            (goto Closed)])]
      [(timeout 1)
       (send to-sender 'Ack0)
       (goto Respond0 to-sender to-app)])

    ;; Waiting for a Seq1 message
    (define-state (Respond1 to-sender to-app)
      [from-sender (msg)
        (match msg
          [(list 'Syn p) (goto Respond1 to-sender to-app)] ; ignore Syn
          [(list 'Write port seq content)
           (match seq
             ['Seq0
               (send to-app (list 'AppMessage port content))
               (send to-sender 'Ack0)
               (goto Respond0 to-sender to-app)]
             ['Seq1 (goto Respond1 to-sender to-app)])]
          [(list 'Fin p)
           (send to-sender 'FinAck)
           (goto Closed)])]
      [(timeout 1)
       (send to-sender 'Ack1)
       (goto Respond1 to-sender to-app)])

    ;; The session is closed; receiver will not acknowledge further messages
    (define-state (Closed)
      [from-sender (msg) (goto Closed)]))

   (list from-sender)))

;; The sending side of an ABTP session
(define (Sender to-recvr port status)
  (spawn-agent
   ((write close from-recvr)

    (begin
      (send to-recvr (list 'Syn port from-recvr))
      (goto SynSent to-recvr port status))

    ;; Waiting for acknowledgment of the SYN
    (define-state (SynSent to-recvr port status)
      ;; NOTE: we use goto-this-state as an obvious shorthand
      [write (m) (goto-this-state)]
      [close (m) (goto-this-state)]
      [from-recvr (m)
        (match m
          ['SynAck
           (send status (list 'Connected write close))
           (goto Ready 'Seq0 to-recvr port status)]
          ;; NOTE: we use goto-this-state as an obvious shorthand
          ['Ack1 (goto SynSent to-recvr port status)]
          ['Ack0 (goto SynSent to-recvr port status)]
          ['FinAck (goto SynSent to-recvr port status)])]
      [(timeout 3)
        (send status 'ConnectFailed)
        (goto ClosedState)])

    ;; Waiting for the application to request messages to write on the session
    (define-state (Ready current-seq to-recvr port status)
      [write (response)
        (send to-recvr (list 'Write port current-seq 'Unit))
        (send response 'Queued)
        (goto AwaitingAck current-seq 1 'Empty 'Empty to-recvr port status)]
      [close (c)
        (send to-recvr (list 'Fin port))
        (goto Closing status)]
      [from-recvr (m)
        ;; can ignore all receiver messages in this state
        (goto Ready current-seq to-recvr port status)])

    ;; Awaiting an acknowledgment from the receiver for a session message
    (define-state (AwaitingAck last-sent-seq
                               send-attempts
                               ;; these two stacks implement a queue in the purely functional style
                               enqueue-stack
                               dequeue-stack
                               to-recvr
                               port
                               status)
      [write (r)
        (send r 'Queued)
        (goto AwaitingAck last-sent-seq send-attempts (list 'Stack enqueue-stack) dequeue-stack to-recvr port status)]
      [close (c)
        (send to-recvr (list 'Fin port))
        (goto Closing status)]
      [from-recvr (m)
        (match m
          ['SynAck (goto-this-state)]
          ['Ack1
            (match last-sent-seq
              ['Seq0 (goto-this-state)]
              ['Seq1
                ;; check for any queued messages to send
                (match dequeue-stack
                  ['Empty (goto Rebalancing 'Seq0 enqueue-stack dequeue-stack to-recvr port status)]
                  [(list 'Stack dequeue-stack)
                    (send to-recvr (list 'Write port 'Seq0 'Unit))
                    (goto AwaitingAck 'Seq0 1 enqueue-stack dequeue-stack to-recvr port status)])])]
          ['Ack0
            (match last-sent-seq
              ['Seq0
                (match dequeue-stack
                  ['Empty (goto Rebalancing 'Seq1 enqueue-stack dequeue-stack to-recvr port status)]
                  [(list 'Stack dequeue-stack)
                    (send to-recvr (list 'Write port 'Seq1 'Unit))
                    (goto AwaitingAck 'Seq1 1 enqueue-stack dequeue-stack to-recvr port status)])]
              ['Seq1 (goto-this-state)])]
          ['FinAck (goto-this-state)])]
      [(timeout 3)
       (match (< send-attempts 3)
         ['True
          (send to-recvr (list 'Write port last-sent-seq 'Unit))
          (goto AwaitingAck last-sent-seq (+ 1 send-attempts) enqueue-stack dequeue-stack to-recvr port status)]
         ['False
          (send status 'ErrorClosed)
          (goto ClosedState)])])

    ;; The state in which we do the rebalancing of the enqueue and dequeue stacks to implement the
    ;; purely functional queue (happens after receving an ACK). When the rebalancing is done, we
    ;; either send the next thing in the queue or go back to Ready if the queue is empty.
    (define-state (Rebalancing current-seq enqueue-stack dequeue-stack to-recvr port status)
      [(timeout 0)
        (match enqueue-stack
          ['Empty
            (match dequeue-stack
              ['Empty (goto Ready current-seq to-recvr port status)]
              [(list 'Stack dequeue-stack)
                (send to-recvr (list 'Write port current-seq 'Unit))
                (goto AwaitingAck current-seq 1 enqueue-stack dequeue-stack to-recvr port status)])]
          [(list 'Stack enqueue-stack)
            (goto Rebalancing current-seq enqueue-stack (list 'Stack dequeue-stack) to-recvr port status)])]
      [write (r)
        (send r 'Queued)
        ;; NOTE: we enqueue the message into the middle of the stack, because we don't have a better
        ;; choice. The lack of recursion means we can't rebalance the entire queue all at once without
        ;; listening to other channels, and because we have no selective receive, we have to listen to
        ;; this channel.
        (goto Rebalancing current-seq enqueue-stack (list 'Stack dequeue-stack) to-recvr port status)]
      [close (c)
        (send to-recvr (list 'Fin port))
        (goto Closing status)]
      [from-recvr (m)
        ;; can ignore all receiver messages in this state
        (goto-this-state)])

    ;; Waiting for a FIN/ACK from the receiver. If not received in time, will close with an error
    ;; status message instead.
    (define-state (Closing status)
      [write (r)
        (send r 'WriteFailed)
        (goto Closing status)]
      [close (c)
        ;; just ignore this
        (goto-this-state)]
      [from-recvr (m)
        (match m
          ['SynAck (goto-this-state)]
          ['Ack0 (goto-this-state)]
          ['Ack1 (goto-this-state)]
          ['FinAck
           (send status 'Closed)
           (goto ClosedState)])]
      [(timeout 3)
        (send status 'ErrorClosed)
        (goto ClosedState)])

    ;; The final state for the session - no further commands or receiver messages are accepted.
    (define-state (ClosedState)
      [write (r)
        (send r 'WriteFailed)
        (goto-this-state)]
      [close (c)
        (goto-this-state)]
      [from-recvr (m) (goto-this-state)]))

   (list write close from-recvr)))

;; Manages a collection of senders and receivers
(define (Manager to-app nic-registration)
  (spawn-agent
   ((connect from-net)

    (begin
      (send nic-registration from-net)
      (goto Ready to-app 'Empty))

    ;; Ready to accept commands or network messages
    (define-state (Ready to-app receivers)
      [connect (req)
        (match req
          [(list 'Connect to-recvr port status)
            ;; We use spawn-named-agent as a small hack to allow separate definition of agents; this
            ;; is effectively equivalent to copy-pasting the the Sender code here into a spawn-agent
            ;; form
            (spawn-named-agent (write close from-recvr)
              (Sender to-recvr port status)
              (goto-this-state))])]
      [from-net (m)
        ;; We assume that any Syn message does not yet have a corresponding Receiver started
        (match m
          [(list 'Syn port to-sender)
            (spawn-named-agent (from-sender) (Receiver to-sender to-app)
              (goto Ready to-app (list 'Cons (list port from-sender) receivers)))]
          [(list 'Write port seq content)
            (goto LookingForReceiver m port receivers to-app receivers)]
          [(list 'Fin port)
            (goto LookingForReceiver m port receivers to-app receivers)])])

    ;; The state in which we examine the receiver list one at a time to match a given port number
    (define-state (LookingForReceiver message port remaining to-app receivers)
      ;; NOTE: we just throw away message in this state, because of the lack of selective
      ;; receive. There are better, more sophisticated ways to handle this
      [connect (m) (goto-this-state)]
      [from-net (m) (goto-this-state)]
      [(timeout 0)
        (match remaining
          ['Empty (goto Ready to-app receivers)]
          [(list 'Cons (list other-port chan) rest)
           (match (= other-port port)
             ['True
              (send chan message)
              (goto Ready to-app receivers)]
             ['False (goto LookingForReceiver message rest to-app receivers)])])]))

   (list connect from-net)))

;; ---------------------------------------------------------------------------------------------------
;; Specifications

;; Defines the behavior of the ABTP manager, from the consumer's point of view
(define-spec ManagerSpec (connect)
  (define-state (Always)
    [connect (list 'Connect * * status) ->
             (Always)
             (activ [s SenderSpec (Connecting status)])]))

;; Defines the behavior of an ABTP sender, from the point of view of the application layer
(define-spec SenderSpec (write close)
  (define-state (Connecting status)
    [unobs ->
      (Closed) (out [status 'ConnectFailed])]
    [unobs ->
      (Connected status)
      (out
       [status
        (list 'Connected (spec-chan self write)
                         (spec-chan self close))])])
  (define-state (Connected status)
    [unobs -> (Closed) (out [status 'ErrorClosed])]
    [write r -> (Connected status) (out [r 'Queued])]
    [close * -> (Closing status)])
  (define-state (Closing status)
    [write r -> (Closing status) (out [r 'WriteFailed])]
    [close * -> (Closing status)]
    [unobs -> (Closed) (out [status 'ErrorClosed])]
    [unobs -> (Closed) (out [status 'Closed])])
  (define-state (Closed)
    [write r -> (Closed) (out [r 'WriteFailed])]
    [close * -> (Closed)]))
