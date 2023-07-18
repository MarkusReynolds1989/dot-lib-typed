#lang racket/base
(require racket/stream)

(define (seq-append seq-one seq-two)
  (stream-append seq-one seq-two))

(define (seq-create . values)
  (stream* values))

(define (seq-fold function accumulator sequence)
    (stream-fold function accumulated sequence))

(define (seq-head sequence)
  (stream-first sequence))

(define (seq-map function sequence)
  (stream-map function sequence))

(provide all-defined-out)

(module+ test
  (require rackunit
           threading)

  (check-eq? 1 (seq-head (seq-create 1 2 3)))
  (check-equal? (stream->list (stream 1 2 3)) (stream->list (seq-create 1 2 3)))
  (check-equal? (stream->list (seq-create 1 2 3 4 5))
                (stream->list (seq-append (seq-create 1 2) (seq-create 3 4 5))))
  (check-equal? (stream->list (seq-create 3 4 5))
                (stream->list (~>> (seq-create 2 3 4) (seq-map (lambda (x) (+ x 1)))))))
