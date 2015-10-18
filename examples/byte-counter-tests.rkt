#lang racket

;; Tests for the Switch agent

;; ---------------------------------------------------------------------------------------------------

(require "byte-counter.rkt")
(require rackunit)
(require asyncunit)
(require racket/async-channel)

(test-case
 "First send works ok"
 (define target (make-async-channel))
 (define agent (ByteCounter))
 (async-channel-put agent (list 'NewTarget target))
 (check-no-message target)
 (async-channel-put agent (list 'Payload "foo"))
 (check-unicast target (list 'Packet 3 "foo")))

(test-case
 "Can send multiple messages to same target"
 (define target (make-async-channel))
 (define agent (ByteCounter))
 (async-channel-put agent (list 'NewTarget target))
 (async-channel-put agent (list 'Payload "foo"))
 (async-channel-put agent (list 'Payload "a"))
 (async-channel-put agent (list 'Payload "abcdefg"))
 (check-unicast target (list 'Packet 3 "foo"))
 (check-unicast target (list 'Packet 1 "a"))
 (check-unicast target (list 'Packet 7 "abcdefg")))

(test-case
 "Can choose new target and receive messages there"
 (define target1 (make-async-channel))
 (define target2 (make-async-channel))
 (define agent (ByteCounter))
 (async-channel-put agent (list 'NewTarget target1))
 (async-channel-put agent (list 'Payload "foo"))
 (async-channel-put agent (list 'NewTarget target2))
 (async-channel-put agent (list 'Payload "bar"))
 (check-unicast target1 (list 'Packet 3 "foo"))
 (check-unicast target2 (list 'Packet 3 "bar")))

(test-case
 "Suspend temporarily suspends output; Resume resumes it"
 (define target (make-async-channel))
 (define agent (ByteCounter))
 (async-channel-put agent (list 'NewTarget target))
 (async-channel-put agent 'Suspend)
 (async-channel-put agent (list 'Payload "foo"))
 (check-no-message target)
 (async-channel-put agent 'Resume)
 (async-channel-put agent (list 'Payload "bar"))
 (check-unicast target (list 'Packet 3 "bar")))
