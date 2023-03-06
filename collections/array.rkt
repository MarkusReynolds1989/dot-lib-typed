#lang typed/racket/base/shallow

; TODO: array-all-pairs

(: array-append (All (T) (-> (Vectorof T) (Vectorof T) (Vectorof T))))
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

(: array-contains (All (T) (-> T (Vectorof T) Boolean)))
(define (array-contains value array)
  (let ([result
         :
         Boolean
         #f])
    (for ([index (in-range 0 (array-length array))])
      (when (equal? value (array-get index array))
        (set! result #t)))
    result))

(: array-copy (All (T) (-> (Vectorof T) (Vectorof T))))
(define (array-copy array)
  (: new-array (Vectorof T))
  (define new-array (array-create (array-length array) (array-get 0 array)))
  (vector-copy! new-array 0 array 0 (array-length array))

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
  (if (= (array-length array) 1) (array-get 0 array) #f))

; TODO: array-except

(: array-exists (All (T) (-> (-> T Boolean) (Vectorof T) Boolean)))
(define (array-exists predicate array)
  (: result Boolean)
  (define result #f)
  (for ([i (in-range 0 (array-length array))])
    (when (predicate (array-get i array))
      (set! result #t)))

  result)

; TODO: array-exists-two

; TODO: array-fill target target-index count value

(: array-filter (All (T) (-> (-> T Boolean) (Vectorof T) (Vectorof T))))
(define (array-filter predicate array)
  (array-fold (lambda ([acc : (Vectorof T)] [item : T])
                (if (predicate item) (array-append (vector item) acc) acc))
              (array-create 0 (array-get 0 array))
              array))

(: array-fold (All (T V) (-> (-> V T V) V (Vectorof T) V)))
(define (array-fold folder state array)
  (let loop ([index 0] [folder folder] [state state] [array array])
    (cond
      [(= index (array-length array)) state]
      [else (loop (+ 1 index) folder (folder state (array-get index array)) array)])))

; TODO: array-fold-two

; TODO: array-fold-back

; TODO: array-fold-back-two

(: array-for-all (All (T) (-> (-> T Boolean) (Vectorof T) Boolean)))
(define (array-for-all predicate array)
  (let loop ([index 0] [predicate predicate] [array array])
    (cond
      [(= index (array-length array)) #t]
      [(not (predicate (array-get index array))) #f]
      [else (loop (+ index 1) predicate array)])))

; TODO: array-for-all-two

(: array-get (All (T) (-> Integer (Vectorof T) T)))
(define (array-get index array)
  (vector-ref array index))

; TODO: array-group-by

(: array-head (All (T) (-> (Vectorof T) T)))
(define (array-head array)
  (array-get 0 array))

;(: array-indexed (All (T) (-> (Vectorof (List Integer T)))))

(: array-init (All (T) (-> Integer (-> Integer T) (Vectorof T))))
(define (array-init count initializer)
  (for/vector :
    (Vectorof T)
    ([index (in-range 0 count)])
    (initializer index)))

; TODO: array-insert-at

; TODO: array-insert-many-at

; TODO: array-is-empty?
(: array-is-empty? (All (T) (-> (Vectorof T) Boolean)))
(define (array-is-empty? array)
  (= (array-length array) 0))

; TODO: array-item is the same as array-get, may not do it.

(: array-iter (All (T) (-> (-> T Void) (Vectorof T) Void)))
(define (array-iter action array)
  (for ([index (in-range 0 (array-length array))])
    (action (array-get index array))))

(: array-iter-two (All (T V) (-> (-> T V Void) (Vectorof T) (Vectorof V) Void)))
(define (array-iter-two action array-one array-two)
  (for ([index (in-range 0 (array-length array-one))])
    (action (array-get index array-one) (array-get index array-two))))

(: array-iter-index (All (T) (-> (-> Integer T Void) (Vectorof T) Void)))
(define (array-iter-index action array)
  (for ([index (in-range 0 (array-length array))])
    (action index (array-get index array))))

(: array-iter-index-two (All (T V) (-> (-> Integer T V Void) (Vectorof T) (Vectorof V) Void)))
(define (array-iter-index-two action array-one array-two)
  (for ([index (in-range 0 (array-length array-one))])
    (action index (array-get index array-one) (array-get index array-two))))

(: array-last (All (T) (-> (Vectorof T) T)))
(define (array-last array)
  (array-get (- (array-length array) 1) array))

(: array-length (All (T) (-> (Vectorof T) Integer)))
(define (array-length array)
  (vector-length array))

(: array-map (All (T V) (-> (-> T V) (Vectorof T) (Vectorof V))))
(define (array-map mapper array)
  (for/vector :
    (Vectorof V)
    ([item array])
    (mapper item)))

(:)

(: array-sum (-> (Vectorof Number) Number))
(define (array-sum array)
  (array-fold (lambda ([acc : Number] [item : Number]) (+ acc item)) 0 array))

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

  (check-equal? (array-filter (lambda ([item : Integer]) (= item 1)) (vector 1 1 1 2 2 2))
                (vector 1 1 1))

  (check-equal? (array-filter (lambda ([item : String]) (> (string-length item) 3))
                              (vector "one" "two" "three"))
                (vector "three"))

  (check-true (array-for-all (lambda ([item : Integer]) (> item 2)) (vector 3 4 5 6 7)))
  (check-false (array-for-all (lambda ([item : Integer]) (> item 2)) (vector 1 2 3 4 5)))
  (check-eq? (array-get 0 (vector 1 2 3 4)) 1)
  (check-equal? (array-map (lambda ([item : Integer]) (+ 1 item)) (vector 1 2 3 4)) (vector 2 3 4 5))
  (check-equal? (array-map (lambda ([item : String]) (string-length item))
                           (vector "one" "two" "three"))
                (vector 3 3 5))

  (check-equal? (array-init 3 (lambda ([item : Number]) (+ item 1))) (vector 1 2 3))

  (check-true (array-is-empty? (vector)))
  (check-false (array-is-empty? (vector 1)))

  (check-eq? (array-last (vector 1 2 3 4)) 4))
