#lang racket

;; Tests for the Switch agent

;; ---------------------------------------------------------------------------------------------------

(require "switch.rkt")
(require rackunit)
(require asyncunit)
(require racket/async-channel)

(test-case
 "On by default"
 (define out (make-async-channel))
 (define switch (Switch out))
 (async-channel-put switch (list 'In 1))
 (check-unicast out 1))

(test-case
 "Toggling once turns the switch off (messages are blocked)"
 (define out (make-async-channel))
 (define switch (Switch out))
 (async-channel-put switch 'Toggle)
 (sleep 0.1)
 (async-channel-put switch (list 'In 1))
 (check-no-message out))

(test-case
 "Toggling once turns off, toggling again turns on, no messages released"
 (define out (make-async-channel))
 (define switch (Switch out))
 (async-channel-put switch 'Toggle)
 (sleep 0.1)
 (async-channel-put switch (list 'In 1))
 (check-no-message out)
 (async-channel-put switch 'Toggle)
 (check-no-message out))

(test-case
 "Toggling twice turns toggle off and back on"
 (define out (make-async-channel))
 (define switch (Switch out))
 (async-channel-put switch 'Toggle)
 (async-channel-put switch 'Toggle)
 (sleep 0.1)
 (async-channel-put switch (list 'In 1))
 (check-unicast out 1))

(test-case
 "Can change output while in On"
 (define switch (Switch (make-async-channel)))
 (define out2 (make-async-channel))
 (async-channel-put switch (list 'NewOut out2))
 (sleep 0.1)
 (async-channel-put switch (list 'In 1))
 (check-unicast out2 1))

(test-case
 "Can change output while in Off"
 (define switch (Switch (make-async-channel)))
 (async-channel-put switch 'Toggle)
 (sleep 0.1)
 (define out2 (make-async-channel))
 (async-channel-put switch (list 'NewOut out2))
 (sleep 0.1)
 (async-channel-put switch 'Toggle)
 (sleep 0.1)
 (async-channel-put switch (list 'In 1))
 (check-unicast out2 1))
