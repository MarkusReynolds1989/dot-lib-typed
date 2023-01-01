#lang typed/racket/base/deep

; TODO: array-all-pairs

(: array-append (All (T) (-> (Vectorof T) (Vectorof T) (Vectorof T))))
(define (array-append array-one array-two)
  (let ([new-array (make-vector (+ (vector-length array-one) (vector-length array-two))
                                (vector-ref array-one 1))])
    (vector-copy! new-array 0 array-one 0 (vector-length array-one))
    (vector-copy! new-array (vector-length array-one) array-two 0 (vector-length array-two))
    new-array))

; Come back after sum.
;(: array-average (-> (Vectorof Number) Number))
;(define (array-average source)
;  )

; TODO: array-average-by

; TODO: array-blit, this is just vector-copy!

; TODO: array-choose

; TODO: array-chunk-by-size

; TODO: array-collect - implement map and concat

; TODO: array-compare-with

; TODO: array-concat

(: array-contains (All (T) (-> T (Vectorof T) Boolean)))
(define (array-contains value source)
  (let ([result : Boolean #f])
    (for ([i (in-range 0 (vector-length source))])
    (when (equal? value (vector-ref source i)) (set! result #t)))
    result))

(module+ test
  (require typed/rackunit)

  (check-equal? (array-append (vector 1 2 3) (vector 4 5 6)) (vector 1 2 3 4 5 6))
  (check-true (array-contains -100 (vector 1 2 3 4 100 23 -100)))
  (check-false (array-contains "blue" (vector "red" "yellow" "green"))))
