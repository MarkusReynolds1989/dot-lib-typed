#lang typed/racket

; TODO: array-all-pairs

(: array-append (All (T) (-> (Vectorof T) (Vectorof T) (Vectorof T))))
(define (array-append array-one array-two)
  (let ([new-array (make-vector (+ (vector-length array-one) (vector-length array-two))
                                (vector-ref array-one 1))])
    (vector-copy! new-array 0 array-one 0 (vector-length array-one))
    (vector-copy! new-array (vector-length array-one) array-two 0 (vector-length array-two))
    new-array))

(module+ test
  (require typed/rackunit)

  (check-equal? (array-append #(1 2 3) #(4 5 6)) #(1 2 3 4 5 6)))
