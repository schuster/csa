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
 (define switch (csa-program out))
 (async-channel-put switch '(variant In 1))
 (check-unicast out 1))

(test-case
 "Toggling once turns the switch off (messages are blocked)"
 (define out (make-async-channel))
 (define switch (csa-program out))
 (async-channel-put switch '(variant Toggle))
 (sleep 0.1)
 (async-channel-put switch '(variant In 1))
 (check-no-message out))

(test-case
 "Toggling once turns off, toggling again turns on, no messages released"
 (define out (make-async-channel))
 (define switch (csa-program out))
 (async-channel-put switch '(variant Toggle))
 (sleep 0.1)
 (async-channel-put switch '(variant In 1))
 (check-no-message out)
 (async-channel-put switch '(variant Toggle))
 (check-no-message out))

(test-case
 "Toggling twice turns toggle off and back on"
 (define out (make-async-channel))
 (define switch (csa-program out))
 (async-channel-put switch '(variant Toggle))
 (async-channel-put switch '(variant Toggle))
 (sleep 0.1)
 (async-channel-put switch '(variant In 1))
 (check-unicast out 1))

(test-case
 "Can change output while in On"
 (define switch (csa-program (make-async-channel)))
 (define out2 (make-async-channel))
 (async-channel-put switch `(variant NewOut ,out2))
 (sleep 0.1)
 (async-channel-put switch '(variant In 1))
 (check-unicast out2 1))

(test-case
 "Can change output while in Off"
 (define switch (csa-program (make-async-channel)))
 (async-channel-put switch '(variant Toggle))
 (sleep 0.1)
 (define out2 (make-async-channel))
 (async-channel-put switch `(variant NewOut ,out2))
 (sleep 0.1)
 (async-channel-put switch '(variant Toggle))
 (sleep 0.1)
 (async-channel-put switch '(variant In 1))
 (check-unicast out2 1))
