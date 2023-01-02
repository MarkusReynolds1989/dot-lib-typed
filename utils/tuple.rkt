#lang typed/racket/base/deep

(: tuple-frst (All (T V) (-> (Pair T V) T)))
(define (tuple-frst tuple)
  (car tuple))

; TODO: Not correct.
(: tuple-snd (All (T V) (-> (Pair T V) V)))
(define (tuple-snd tuple)
  (cdr tuple))

(provide (all-defined-out))
