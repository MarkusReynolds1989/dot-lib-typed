#lang racket/base

(require rackunit
         "../collections/array.rkt")

; Array is the actual collection.
; Index is the current index we are on, inc to add a new item.
; Size is how many total the array can hold.
; Count is how many items are actually in the array.
(struct resize-array (array index size count) #:mutable)

(define (make-resize-array [size 256])
  (resize-array (Array-create size '()) -1 size 0))

; Create a new array of size: size * 2.
; Copy all the items from the first array into the new one.
; Mutates resize-array so it takes the new size and the new array.
(define (grow resize-array)
  (define size (* (resize-array-size resize-array) 2))
  (define out-array (make-resize-array size))
  (set-resize-array-array! resize-array (Array-copy-to resize-array out-array))
  (set-resize-array-size! resize-array (array-length out-array)))

(define (resize-array-add resize-array item)
  (define index (+ (resize-array-index resize-array) 1))
  (define count (+ (resize-array-count resize-array) 1))
  (define size (resize-array-size resize-array))

  (when (> count size)
    (grow resize-array))

  (set-resize-array-index! index)
  (vector-set! resize-array index item))

(provide make-resize-array resize-array-add)

(module+ test
  (define people (make-resize-array))
  (check-equal? (resize-array-array people) (Array-create 256 '())))
