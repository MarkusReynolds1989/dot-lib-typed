#lang racket/base

; TODO: array-all-pairs

(define (array-append array-one array-two)
  (let ([new-array (make-vector (+ (array-length array-one) (vector-length array-two))
                                (array-get 0 array-one))])
    (vector-copy! new-array 0 array-one 0 (array-length array-one))
    (vector-copy! new-array (array-length array-one) array-two 0 (vector-length array-two))
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

(define (array-contains value array)
  (let ([result #f])
    (for ([index (in-range 0 (array-length array))])
      (when (equal? value (array-get index array))
        (set! result #t)))
    result))

(define (array-copy array)
  (define new-array (array-create (array-length array) (array-get 0 array)))
  (vector-copy! new-array 0 array 0 (array-length array))

  new-array)

; TODO: array-count-by

; TODO: array-create
(define (array-create count init-value)
  (make-vector count init-value))

; TODO: array-distinct - just a hashmap or set if they have it.

; TODO: array-distinct-by

(define (array-empty)
  (vector))

(define (array-exactly-one array)
  (if (= (array-length array) 1) (array-get 0 array) #f))

; TODO: array-except

(define (array-exists predicate array)
  (define result #f)
  (for ([i (in-range 0 (array-length array))])
    (when (predicate (array-get i array))
      (set! result #t)))

  result)

; TODO: array-exists-two

; TODO: array-fill target target-index count value

(define (array-filter predicate array)
  (array-fold (lambda (acc item) (if (predicate item) (array-append (vector item) acc) acc))
              (array-create 0 (array-get 0 array))
              array))

(define (array-fold folder state array)
  (let loop ([index 0] [folder folder] [state state] [array array])
    (cond
      [(= index (array-length array)) state]
      [else (loop (+ 1 index) folder (folder state (array-get index array)) array)])))

; TODO: array-fold-two

; TODO: array-fold-back

; TODO: array-fold-back-two

(define (array-for-all predicate array)
  (let loop ([index 0] [predicate predicate] [array array])
    (cond
      [(= index (array-length array)) #t]
      [(not (predicate (array-get index array))) #f]
      [else (loop (+ index 1) predicate array)])))

; TODO: array-for-all-two

(define (array-get index array)
  (vector-ref array index))

; TODO: array-group-by

(define (array-head array)
  (array-get 0 array))

(define (array-init count initializer)
  (for/vector ([index (in-range 0 count)])
    (initializer index)))

; TODO: array-insert-at

; TODO: array-insert-many-at

; TODO: array-is-empty?
(define (array-is-empty? array)
  (= (array-length array) 0))

; TODO: array-item is the same as array-get, may not do it.

(define (array-iter action array)
  (for ([index (in-range 0 (array-length array))])
    (action (array-get index array))))

(define (array-iter-two action array-one array-two)
  (for ([index (in-range 0 (array-length array-one))])
    (action (array-get index array-one) (array-get index array-two))))

(define (array-iter-index action array)
  (for ([index (in-range 0 (array-length array))])
    (action index (array-get index array))))

(define (array-iter-index-two action array-one array-two)
  (for ([index (in-range 0 (array-length array-one))])
    (action index (array-get index array-one) (array-get index array-two))))

(define (array-last array)
  (array-get (- (array-length array) 1) array))

(define (array-length array)
  (vector-length array))

(define (array-map mapper array)
  (for/vector ([item array])
    (mapper item)))

(define (array-sum array)
  (array-fold (lambda (acc item) (+ acc item)) 0 array))

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
  (check-true (array-exists (lambda (item) (= item 1)) (vector 1 2 3 4)))
  (check-false (array-exists (lambda (item) (equal? item "blue")) (vector "red" "yellow" "green")))

  (check-eq? (array-fold (lambda (acc item) (+ acc item)) 0 (vector 1 2 3 4)) 10)

  (check-equal? (array-filter (lambda (item) (= item 1)) (vector 1 1 1 2 2 2)) (vector 1 1 1))

  (check-equal? (array-filter (lambda (item) (> (string-length item) 3)) (vector "one" "two" "three"))
                (vector "three"))

  (check-true (array-for-all (lambda (item) (> item 2)) (vector 3 4 5 6 7)))
  (check-false (array-for-all (lambda (item) (> item 2)) (vector 1 2 3 4 5)))
  (check-eq? (array-get 0 (vector 1 2 3 4)) 1)
  (check-equal? (array-map (lambda (item) (+ 1 item)) (vector 1 2 3 4)) (vector 2 3 4 5))
  (check-equal? (array-map (lambda (item) (string-length item)) (vector "one" "two" "three"))
                (vector 3 3 5))

  (check-equal? (array-init 3 (lambda (item) (+ item 1))) (vector 1 2 3))

  (check-true (array-is-empty? (vector)))
  (check-false (array-is-empty? (vector 1)))

  (check-eq? (array-last (vector 1 2 3 4)) 4))
