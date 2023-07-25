#lang typed/racket/base/deep

(define-syntax-rule (fn args body ...)
  (lambda args
    body ...))

(define min-int -217483648)
(define max-int 217483648)

(define-type (Maybe T) (U T False))
;(define-type (Result T) (U T String))

(provide fn
         max-int
         min-int
         Maybe)
