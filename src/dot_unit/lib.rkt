#lang typed/racket/base

(require typed/rackunit)

(define-syntax-rule (fact name body ...)
  (test-case name
    body ...))

(define assert-true check-true)
(define assert-equal check-equal?)

(fact "1 = 1" (assert-true (= 1 1)))
(fact "'one' = 'one'" (assert-equal "one" "one"))
