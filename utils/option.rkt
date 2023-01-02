#lang typed/racket/base/deep

(: unwrap-or (All (T) (-> T (U T False) T)))
(define (unwrap-or default value)
  (if (equal? value #f) default value))

(provide (all-defined-out))