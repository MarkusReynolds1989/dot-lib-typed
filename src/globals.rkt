#lang typed/racket/base

(define-syntax-rule (fn args body ...)
  (lambda args
    body ...))

(define min-int -217483648)
(define max-int 217483648)

(struct (T S) Tuple ([First : T] [Second : S]) #:transparent)

(struct None ())
(struct (T) Some ([v : T]))

(define-type (Maybe T) (U None (Some T)))
;(define-type (Result T) (U T String))

(provide (all-defined-out))
