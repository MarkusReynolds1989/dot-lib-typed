#lang racket/base
(require racket/stream)

(define (seq-create . values)
    (stream* values))

(define (seq-head sequence)
    (stream-first sequence))

(provide all-defined-out)

(module+ test
    (require rackunit threading)
    (check-eq? 1 (seq-head (seq-create 1 2 3)))
    (check-equal? (stream->list (stream 1 2 3)) (stream->list (seq-create 1 2 3))))
