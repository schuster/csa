#lang racket

;; Tests for the Switch agent

;; ---------------------------------------------------------------------------------------------------

(require "character-count.rkt")
(require rackunit)
(require asyncunit)
(require racket/async-channel)

(test-case
 "First send works ok"
 (define target (make-async-channel))
 (define agent (CharacterCounter))
 (async-channel-put agent (list 'NewTarget target))
 (check-no-message target)
 (async-channel-put agent (list 'Message "foo"))
 (check-unicast target (list 'WrappedMessage 3 "foo")))

(test-case
 "Can send multiple messages to same target"
 (define target (make-async-channel))
 (define agent (CharacterCounter))
 (async-channel-put agent (list 'NewTarget target))
 (async-channel-put agent (list 'Message "foo"))
 (async-channel-put agent (list 'Message "a"))
 (async-channel-put agent (list 'Message "abcdefg"))
 (check-unicast target (list 'WrappedMessage 3 "foo"))
 (check-unicast target (list 'WrappedMessage 1 "a"))
 (check-unicast target (list 'WrappedMessage 7 "abcdefg")))

(test-case
 "Can choose new target and receive messages there"
 (define target1 (make-async-channel))
 (define target2 (make-async-channel))
 (define agent (CharacterCounter))
 (async-channel-put agent (list 'NewTarget target1))
 (async-channel-put agent (list 'Message "foo"))
 (async-channel-put agent (list 'NewTarget target2))
 (async-channel-put agent (list 'Message "bar"))
 (check-unicast target1 (list 'WrappedMessage 3 "foo"))
 (check-unicast target2 (list 'WrappedMessage 3 "bar")))
