#lang typed/racket/base

; TODO: Turn this into a class.
(require (prefix-in Array. "../collections/array.rkt"))

; Array is the actual collection.
; Index is the current index we are on, inc to add a new item.
; Size is how many total the array can hold.
; Count is how many items are actually in the array.
(struct (T)
        Resize-Array
        ([Array : (MutableVector T)] [Index : Integer] [Size : Integer] [Count : Integer])
  #:mutable)

(: new (All (T) (-> Integer (Resize-Array T))))
(define (new [size 256])
  (Resize-Array (Array (ann (Array.create size '()) (MutableVector T)) (Index -1) (Size size 0))))

(define (get index input)
  (Array.get index (Resize-array-array input)))

; Create a new array of size: size * 2.
; Copy all the items from the first array into the new one.
; Mutates resize-array so it takes the new size and the new array.
(define (grow input)
  (define size (* (Resize-array-size input) 2))
  (define output (Resize-array-new size))
  (set-Resize-array-array! input (Array.copy-to (Resize-array-array input) output))
  (set-Resize-array-size! size))

(define (add item input)
  (define index (add1 (Resize-array-index input)))
  (define count (add1 (Resize-array-count input)))
  (define size (Resize-array-size input))

  (when (> (add1 count) size)
    (grow input))

  (set-Resize-array-index! input index)
  (set-Resize-array-count! input count)
  (vector-set! (Resize-array-array input) index item))

(provide new
         add)

(module+ test
  (require rackunit)
  (define people (Resize-array-new))
  (define (big-addition-test input)
    (for-each (lambda (x) (Resize-array-add x input)) (build-list 10000 (lambda (_) 0))))

  (check-equal? (Resize-array-array people) (Array.create 256 '()))
  (check-eq? (Resize-array-size people) 256)
  (check-eq? (Resize-array-count people) 0)
  (Resize-array-add 2 people)
  (check-eq? (Resize-array-get 0 people) 2)
  (big-addition-test people))
