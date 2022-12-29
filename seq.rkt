#lang typed/racket/base
(require/typed racket/stream
               [stream-empty? (All (T) ((Sequenceof T) -> Boolean))])

(provide stream-empty?
         stream)

(stream-empty? (stream 1 2 3))

(module+ test
  (require typed/rackunit))
