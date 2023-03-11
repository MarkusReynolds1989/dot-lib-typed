#lang racket/base

(define (map-add key value table)
  (hash-set table key value))

(define (map-change key f table)
  (hash-update table key f))

(module+ test
  (require rackunit)
  (define table (hash 1 "one"))
  (check-equal? (map-add 2 "two" table) (hash 1 "one" 2 "two"))
  (check-equal? (map-change 1 (lambda (_) "three") table) (hash 1 "three")))
