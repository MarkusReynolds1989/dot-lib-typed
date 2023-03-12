#lang racket/base

; TODO: array-all-pairs

(define (Array-append array-one array-two)
  (let ([new-array (make-vector (+ (Array-length array-one) (vector-length array-two))
                                (Array-get 0 array-one))])
    (vector-copy! new-array 0 array-one 0 (Array-length array-one))
    (vector-copy! new-array (Array-length array-one) array-two 0 (vector-length array-two))
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

(define (Array-contains value array)
  (let ([result #f])
    (for ([index (in-range 0 (Array-length array))])
      (when (equal? value (Array-get index array))
        (set! result #t)))
    result))

(define (Array-copy array)
  (define new-array (Array-create (Array-length array) (Array-get 0 array)))
  (vector-copy! new-array 0 array 0 (Array-length array))

  new-array)

(define (Array-copy-to in-array out-array)
  (vector-copy! out-array 0 in-array 0 (Array-length out-array)))

; TODO: array-count-by

; TODO: Array-create
(define (Array-create count init-value)
  (make-vector count init-value))

; TODO: array-distinct - just a hashmap or set if they have it.

; TODO: array-distinct-by

(define (Array-empty)
  (vector))

(define (Array-exactly-one array)
  (and (= (Array-length array) 1) (Array-get 0 array)))

; TODO: array-except

(define (Array-exists predicate array)
  (define result #f)
  (for ([i (in-range 0 (Array-length array))])
    (when (predicate (Array-get i array))
      (set! result #t)))

  result)

; TODO: array-exists-two

; TODO: array-fill target target-index count value

(define (Array-filter predicate array)
  (Array-fold (lambda (acc item) (if (predicate item) (Array-append (vector item) acc) acc))
              (Array-create 0 (Array-get 0 array))
              array))

(define (Array-fold folder state array)
  (let loop ([index 0] [folder folder] [state state] [array array])
    (cond
      [(= index (Array-length array)) state]
      [else (loop (+ 1 index) folder (folder state (Array-get index array)) array)])))

; TODO: array-fold-two

; TODO: array-fold-back

; TODO: array-fold-back-two

(define (Array-for-all predicate array)
  (let loop ([index 0] [predicate predicate] [array array])
    (cond
      [(= index (Array-length array)) #t]
      [(not (predicate (Array-get index array))) #f]
      [else (loop (+ index 1) predicate array)])))

; TODO: array-for-all-two

(define (Array-get index array)
  (vector-ref array index))

; TODO: array-group-by

(define (Array-head array)
  (Array-get 0 array))

(define (Array-init count initializer)
  (for/vector ([index (in-range 0 count)])
    (initializer index)))

; TODO: array-insert-at

; TODO: array-insert-many-at

; TODO: array-is-empty?
(define (Array-is-empty? array)
  (= (Array-length array) 0))

; TODO: array-item is the same as array-get, may not do it.

(define (Array-iter action array)
  (for ([index (in-range 0 (Array-length array))])
    (action (Array-get index array))))

(define (Array-iter-two action array-one array-two)
  (for ([index (in-range 0 (Array-length array-one))])
    (action (Array-get index array-one) (Array-get index array-two))))

(define (Array-iter-index action array)
  (for ([index (in-range 0 (Array-length array))])
    (action index (Array-get index array))))

(define (Array-iter-index-two action array-one array-two)
  (for ([index (in-range 0 (Array-length array-one))])
    (action index (Array-get index array-one) (Array-get index array-two))))

(define (Array-last array)
  (Array-get (- (Array-length array) 1) array))

(define (Array-length array)
  (vector-length array))

(define (Array-map mapper array)
  (for/vector ([item array])
    (mapper item)))

(define (Array-sum array)
  (Array-fold (lambda (acc item) (+ acc item)) 0 array))

(provide (all-defined-out))

(module+ test
  (require typed/rackunit)
  (define old-array (vector 1 2 3 4))

  (check-equal? (Array-append (vector 1 2 3) (vector 4 5 6)) (vector 1 2 3 4 5 6))
  (check-true (Array-contains -100 (vector 1 2 3 4 100 23 -100)))
  (check-false (Array-contains "blue" (vector "red" "yellow" "green")))
  (check-not-eq? old-array (Array-copy old-array))
  (check-equal? (Array-copy old-array) old-array)
  (check-equal? (Array-create 3 0) (vector 0 0 0))
  (check-false (Array-exactly-one (vector 1 2)))
  (check-eq? (Array-exactly-one (vector 1)) 1)
  (check-true (Array-exists (lambda (item) (= item 1)) (vector 1 2 3 4)))
  (check-false (Array-exists (lambda (item) (equal? item "blue")) (vector "red" "yellow" "green")))

  (check-eq? (Array-fold (lambda (acc item) (+ acc item)) 0 (vector 1 2 3 4)) 10)

  (check-equal? (Array-filter (lambda (item) (= item 1)) (vector 1 1 1 2 2 2)) (vector 1 1 1))

  (check-equal? (Array-filter (lambda (item) (> (string-length item) 3)) (vector "one" "two" "three"))
                (vector "three"))

  (check-true (Array-for-all (lambda (item) (> item 2)) (vector 3 4 5 6 7)))
  (check-false (Array-for-all (lambda (item) (> item 2)) (vector 1 2 3 4 5)))
  (check-eq? (Array-get 0 (vector 1 2 3 4)) 1)
  (check-equal? (Array-map (lambda (item) (add1 item)) (vector 1 2 3 4)) (vector 2 3 4 5))
  (check-equal? (Array-map (lambda (item) (string-length item)) (vector "one" "two" "three"))
                (vector 3 3 5))

  (check-equal? (Array-init 3 (lambda (item) (add1 item))) (vector 1 2 3))

  (check-true (Array-is-empty? (vector)))
  (check-false (Array-is-empty? (vector 1)))

  (check-eq? (Array-last (vector 1 2 3 4)) 4))
