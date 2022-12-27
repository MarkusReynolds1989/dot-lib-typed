#lang racket/base
(require racket/stream
         threading)

(define lazy-items (stream 1 2 3))
(~> (stream-map (lambda (x) (+ x 1)) lazy-items)
    (stream-filter (lambda (x) (> x 2)) _)
    (stream-for-each (lambda (x) (print x)) _))
