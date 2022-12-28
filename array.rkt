#lang racket/base
(require racket/vector
         racket/list
         racket/stream)

; Returns a new array that contains all pairings of elements from the first and second arrays.
(define (array-all-pairs array-one array-two)
  (define (loop index)
    (cond
      [(= index (array-length array-one)) '()]
      [(cons (list (array-get index array-one) (array-get index array-two)) (loop (+ index 1)))]))
  (list->vector (loop 0)))

; Builds a new array that contains elements of the first array followed by the elements of the second array.
(define (array-append first-array second-array)
  (vector-append first-array second-array))

; Get the rounded average of the array.
(define (array-average array)
  (define len (vector-length array))
  (define sum (array-sum array))
  (round (/ sum len)))

;
(define (array-average-by projection array)
  (array-average (vector-map projection array)))

; Reads a range of elements from the first array and write them into the second.
; Mutates the second array with elements from the first.
; source: 'T[]
; source-index: int
; target: 'T[]
; taget-index: int
; count: int
(define (array-blit source source-index target target-index count)
  0)

; Applies the given fucntion to each element of the array. Returns teh array comprised of the
; results x for each element where the function returns Some(x).
; chooser: 'T -> 'U option
; array: 'T[]
; Returns 'U[]
(define (array-choose chooser array)
  (vector-map chooser array))

; Divides the input array into chunks of size at most chunk-size.
(define (array-chunk-by-size chunk-size array)
  (define (loop )))

; Gets the first element of the array.
(define (array-head array)
  (array-get 0 array))

(define (array-last array)
  (array-get (- (array-length array) 1) array))

; Builds a new array whose elements are the corresponding elements of hte input array paired with the
; integer index (from 0) of each element.
(define (array-indexed array)
  0)

(define (array-length array)
  (vector-length array))

(define (array-iter action array)
  (for ([i (range 0 (vector-length array))])
    (action (array-get i array))))

(define (array-iter-two action array-one array-two)
  (for ([i (range 0 (vector-length array-one))])
    (action (array-get i array-one) (array-get i array-two))))

(define (array-iter-index action array)
  (for ([i (range 0 (vector-length array))])
    (action i (array-get i array))))

(define (array-iter-index-two action array-one array-two)
  0)

; Returns the greatest of all elements of the array.
; TODO: Some sort of comparator for different types, this will only work for numerics.
(define (array-max array)
  (define (loop array index max)
    (cond
      [(= index (array-length array)) max]
      [(> (array-get index array) max) (loop array (+ 1 index) (array-get index array))]
      [else (loop array (+ 1 index) max)]))
  (loop array 0 (array-head array)))

; Returns the greatest of all elements of the array.
; TODO: Contracts on types and comparator.
(define (array-max-by projection array)
  (define (loop index max)
    (cond
      [(= index (array-length array)) max]
      [(projection array) (loop (+ 1 index) (array-get index array))]
      [else (loop (+ 1 index) max)]))
  (loop 0 (array-head array)))

; Applies a function to each element of the collection, threading an accumulator argument through
; the computation. If the input function is f and the elements are i0...iN then computers
; f(... (f s i0)...) iN
(define (array-fold folder state array)
  (define (loop folder state array counter)

    (cond
      [(>= counter (vector-length array)) state]
      [else (loop folder (folder (array-get counter array) state) array (+ 1 counter))]))

  (loop folder state array 0))

; Gets an element from an array.
(define (array-get index array)
  (vector-ref array index))

(define (array-of-list list)
  (list->vector list))

(define (array-of-seq seq)
  (list->vector (stream->list seq)))

; Applies a key-generating function to each element of an array and yields an array of unique keys.
; Each unique key contains an array of all elements that match to this key.
; Projection: 'T -> 'Key
; Array: 'T[]
; Returns: ('Key * 'T[])[]
(define (array-group-by projection array)
  raise
  "Not implemented.")

(define (array-singleton value)
  (vector value))

(define (array-skip count array)
  (vector-drop array count))

(define (array-sort array)
  (vector-sort array <))

(define (array-sub array start-index count)
  (vector-take (vector-drop array start-index) count))

; Sum all the elements of the array together.
(define (array-sum array)
  (array-fold (lambda (x y) (+ x y)) 0 array))

(define (array-tail array)
  (vector-drop array 1))

(define (array-take count array)
  (vector-take array count))

(define (array->list array)
  (vector->list array))

(define (array->seq array)
  (stream-first (stream (array->list array))))

(module+ test
  (require rackunit)
  (define first-array #(1 2 3 4))
  (define second-array #(5 6 7 8))
  (define combined-arrays #(1 2 3 4 5 6 7 8))

  ; All-pairs test
  (check-equal? (array-all-pairs first-array second-array) (vector '(1 5) '(2 6) '(3 7) '(4 8)))
  ; Append test
  (check-equal? combined-arrays (array-append first-array second-array))
  ; Sum test
  (check-equal? 36 (array-sum combined-arrays))
  ; Average test
  (check-eq? 4 (array-average combined-arrays))
  ; Head test
  (check-eq? 1 (array-head combined-arrays))
  ; Last test
  (check-eq? 8 (array-last combined-arrays))
  ; Max test
  (check-eq? 8 (array-max combined-arrays))
  ; OfList test
  (check-equal? #(1 2 3) (array-of-list (list 1 2 3)))
  ; OfSeq test
  (check-equal? #(1 2 3) (array-of-seq (stream 1 2 3)))
  ; Singleton test
  (check-equal? #(1) (array-singleton 1))
  ; Skip test
  (check-equal? #(3 4) (array-skip 2 #(1 2 3 4)))
  ; Sort test
  (check-equal? (array-sort #(4 2 3 1)) #(1 2 3 4))
  ; Sub test
  (check-equal? (array-sub #(1 2 3 4) 1 2) #(2 3))
  ; Tail test
  (check-equal? (array-tail combined-arrays) #(2 3 4 5 6 7 8))
  ; Take test
  (check-equal? (array-take 2 combined-arrays) #(1 2))
  ; ->list test
  (check-equal? (array->list first-array) (list 1 2 3 4))
  ; ->seq test
  (check-equal? (stream->list (array->seq first-array)) (stream->list (stream 1 2 3 4))))
