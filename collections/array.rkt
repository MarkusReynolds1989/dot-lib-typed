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
(define (array-contains value array)
  (let ([result
         :
         Boolean
         #f])
    (for ([i (in-range 0 (vector-length array))])
      (when (equal? value (vector-ref array i))
        (set! result #t)))
    result))

(: array-copy (All (T) (-> (Vectorof T) (Vectorof T))))
(define (array-copy array)
  (: new-array (Vectorof T))
  (define new-array (array-create (vector-length array) (vector-ref array 0)))
  (vector-copy! new-array 0 array 0 (vector-length array))

  new-array)

; TODO: array-count-by

; TODO: array-create
(: array-create (All (T) (-> Integer T (Vectorof T))))
(define (array-create count init-value)
  (make-vector count init-value))

; TODO: array-distinct - just a hashmap or set if they have it.

; TODO: array-distinct-by

(: array-empty (All (T) (-> (Vectorof T))))
(define (array-empty)
  (vector))

(: array-exactly-one (All (T) (-> (Vectorof T) (Option T))))
(define (array-exactly-one array)
  (if (= (vector-length array) 1) (vector-ref array 0) #f))

; TODO: array-except

(: array-exists (All (T) (-> (-> T Boolean) (Vectorof T) Boolean)))
(define (array-exists predicate array)
  (: result Boolean)
  (define result #f)
  (for ([i (in-range 0 (vector-length array))])
    (when (predicate (vector-ref array i))
      (set! result #t)))

  result)

; TODO: array-exists-two

; TODO: array-fill target target-index count value

(: array-filter (All (T) (-> (-> T Boolean) (Vectorof T) (Vectorof T))))
(define (array-filter predicate array)
  (array-fold (lambda ([acc : (Vectorof T)] [item : T])
                (if (predicate item) (array-append (vector item) acc) acc))
              (array-create (vector-length array) (vector-ref array 0))
              array))

(: array-fold (All (T V) (-> (-> V T V) V (Vectorof T) V)))
(define (array-fold folder state array)
  (let loop ([index 0] [folder folder] [state state] [array array])
    (cond
      [(= index (vector-length array)) state]
      [else (loop (+ 1 index) folder (folder state (vector-ref array index)) array)])))

(module+ test
  (require typed/rackunit)
  (define old-array (vector 1 2 3 4))

  (check-equal? (array-append (vector 1 2 3) (vector 4 5 6)) (vector 1 2 3 4 5 6))
  (check-true (array-contains -100 (vector 1 2 3 4 100 23 -100)))
  (check-false (array-contains "blue" (vector "red" "yellow" "green")))
  (check-not-eq? old-array (array-copy old-array))
  (check-equal? (array-copy old-array) old-array)
  (check-equal? (array-create 3 0) (vector 0 0 0))
  (check-false (array-exactly-one (vector 1 2)))
  (check-eq? (array-exactly-one (vector 1)) 1)
  (check-true (array-exists (lambda ([item : Number]) (= item 1)) (vector 1 2 3 4)))
  (check-false (array-exists (lambda ([item : String]) (equal? item "blue"))
                             (vector "red" "yellow" "green")))

  (check-eq? (array-fold (lambda ([acc : Integer] [item : Integer]) (+ acc item)) 0 (vector 1 2 3 4))
             10)

  (check-equal?
   (array-filter (lambda ([item : String]) (> (string-length item) 3)) (vector "one" "two" "three"))
   (vector "three")))
