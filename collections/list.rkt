; A library that contains all the functions for working with lists.
; Instead of having "try" functions that produce option and regular functions that raise exceptions
; I am defaulting to optional functions.

#lang racket/base
(require "../globals.rkt")

(define (all-pairs list-one list-two)
  (cond
    [(null? (cdr list-one)) (cons (list (car list-one) (car list-two)) '())]
    [else (cons (list (car list-one) (car list-two)) (all-pairs (cdr list-one) (cdr list-two)))]))

(define (append list-one list-two)
  (if (null? list-one) list-two (cons (car list-one) (append (cdr list-one) list-two))))

(define (average source)
  (/ (apply + source) (length source)))

(define (average-by projection source)
  (average (map projection source)))

(define (choose chooser source)
  (raise "Not implemented."))

(define (chunk-by-size chunk-size source)
  (raise "Not implemented."))

(define (collect mapping source)
  (raise "Not implemented"))

(define (compare-with comparer list-one list-two)
  ;;; TODO: Implement a compare function.
  (raise "Not implemented"))

(define (concat lists)
  (raise "Not implemented"))

(define (contains value source)
  (cond
    [(null? source) #f]
    [(equal? (car source) value) #t]
    [else (contains value (cdr source))]))

(define (count-by projection source)
  (raise "Not implemented."))

(define (distinct source)
  (raise "Not implemented."))

(define (distinct-by projection source)
  (raise "Not implemented."))

(define (empty)
  '())

(define (exactly-one source)
  (car source))

(define (exists predicate source)
  (cond
    [(null? source) #f]
    [(predicate (car source)) #t]
    [else (exists predicate (cdr source))]))

(define (exists-two predicate list-one list-two)
  (cond
    [(and (null? list-one) (null? list-two)) #f]
    [(predicate (car list-one) (car list-two)) #t]
    [(null? (cdr list-one)) (exists-two predicate list-one (cdr list-two))]
    [else (exists-two predicate (cdr list-one) list-two)]))

(define (filter predicate source)
  (cond
    [(null? source) '()]
    [(predicate (car source)) (cons (car source) (filter predicate (cdr source)))]
    [else (filter predicate (cdr source))]))

(define (find predicate source)
  (cond
    [(null? source) #f]
    [(predicate (car source)) (car source)]
    [else (find predicate (cdr source))]))

(define (find-back predicate source)
  (define reverse-source (reverse source))
  (cond
    [(null? reverse-source) #f]
    [(predicate (car reverse-source)) (car reverse-source)]
    [else (find predicate (cdr reverse-source))]))

(define (find-index predicate source)
  (let loop ([index 0] [predicate predicate] [source source])
    (cond
      [(null? source) #f]
      [(predicate (car source)) index]
      [else (loop (+ index 1) predicate (cdr source))])))

(define (find-index-back predicate source)
  (define reverse-source (reverse source))
  (define count (- (length source) 1))

  (let loop ([index count] [predicate predicate] [source reverse-source])
    (cond
      [(null? source) #f]
      [(predicate (car source)) index]
      [else (loop (- index 1) predicate (cdr source))])))

(define (fold folder state source)
  (cond
    [(null? source) state]
    [else (fold folder (folder state (car source)) (cdr source))]))

(define (fold-two folder state list-one list-two)
  (cond
    [(and (null? list-one) (null? list-two)) state]
    [(and (null? (cdr list-one)) (not (null? (cdr list-two))))
     (fold-two folder (folder state (car list-one) (car list-two)) list-one (cdr list-two))]
    [(and (null? (cdr list-two)) (not (null? (cdr list-one))))
     (fold-two folder (folder state (car list-one) (car list-two)) (cdr list-one) list-two)]
    [else
     (fold-two folder (folder state (car list-one) (car list-two)) (cdr list-one) (cdr list-two))]))

(define (fold-back folder state source)
  (define reverse-source (reverse source))
  (fold folder state reverse-source))

(define (fold-back-two folder state list-one list-two)
  (define list-one-reverse (reverse list-one))
  (define list-two-reverse (reverse list-two))

  (cond
    [(and (null? list-one-reverse) (null? list-two-reverse)) state]
    [(and (null? (cdr list-one-reverse)) (not (null? (cdr list-two-reverse))))
     (fold-two folder
               (folder state (car list-one-reverse) (car list-two-reverse))
               list-one-reverse
               (cdr list-two-reverse))]
    [(and (null? (cdr list-two-reverse)) (not (null? (cdr list-one-reverse))))
     (fold-two folder
               (folder state (car list-one-reverse) (car list-two-reverse))
               (cdr list-one-reverse)
               list-two-reverse)]
    [else
     (fold-two folder
               (folder state (car list-one-reverse) (car list-two-reverse))
               (cdr list-one-reverse)
               (cdr list-two-reverse))]))

(define (for-all predicate source)
  (if (> (length (filter predicate source)) 0) #t #f))

(define (for-all-two predicate list-one list-two)
  (raise "Not implemented."))

(define (group-by projection source)
  (raise "Not implemented."))

(define (head source)
  (if (null? source) #f (car source)))

(define (indexed source)
  (let loop ([index 0] [source source])
    (cond
      [(null? source) '()]
      [(append (list (cons index (car source))) (loop (+ 1 index) (cdr source)))])))

(define (init count
              initializer)
  (let loop ([index 0] [count count] [initializer initializer])
    (cond
      [(= index count) '()]
      [else (cons (initializer index) (loop (+ 1 index) count initializer))])))

; TODO: Build a splitter.
(define (insert index value source)
  (raise "Not implemented."))

; TODO: list-insert-many-at

(define (is-empty? source)
  (null? source))

(define (item index source)
  (let loop ([acc 0] [index index] [source source])
    (cond
      [(null? source) #f]
      [(= acc index) (car source)]
      [else (loop (+ 1 acc) index (cdr source))])))

(define (iter action source)
  (cond
    [(null? source) (void)]
    [else
     (action (car source))
     (iter action (cdr source))]))

; TODO: Finish all cases and test.
(define (iter-two action list-one list-two)
  (cond
    [(and (null? list-one) (null? list-two)) (void)]
    [(and (null? (cdr list-two)) (not (null? (cdr list-one))))
     (action (car list-one) (car list-two))
     (iter-two action (cdr list-one) list-two)]))

(define (iter-index action source)
  (let loop ([index 0] [action action] [source source])
    (cond
      [(null? source) (void)]
      [else
       (action index (car source))
       (loop (+ 1 index) action (cdr source))])))

(define (iter-index-two action list-one list-two)
  (when (not (= (length list-one) (length list-two)))
    (raise "Arguments must be the same length to list-iter-index-two."))
  (let loop ([index 0] [action action] [list-one list-one] [list-two list-two])
    (cond
      [(null? list-one) (void)]
      [else
       (action index (car list-one) (car list-two))
       (loop (+ 1 index) action (cdr list-one) (cdr list-two))])))

(define (last source)
  (if (null? source) #f (item (- (length source) 1) source)))

(define (length source)
  (if (null? source) 0 (+ 1 (length (cdr source)))))

(define (map mapping source)
  (cond
    [(null? source) '()]
    [else (cons (mapping (car source)) (map mapping (cdr source)))]))

; TODO: map-two
; TODO: map-three
; TODO: map-fold - returns the list and accumulated value as a pair.
; TODO: map-fold-back - same as above but with a reversed list.

(define (map-index mapping source)
  (let loop ([index 0] [mapping mapping] [source source])
    (cond
      [(null? source) '()]
      [else (cons (mapping index (car source)) (loop (+ 1 index) mapping (cdr source)))])))

; TODO: map-index-two - Lists same length.

; TODO: Come back to fix the wonky type checking.
(define (max source)
  (fold (fn (max-num item) (if (< max-num item) item max-num)) -320000 source))

; TODO: list-max-by
; TODO: list-min
; TODO: list-min-by

(define (of-array array)
  (let loop ([index 0] [array array])
    (cond
      [(= index (vector-length array)) '()]
      [else (cons (vector-ref array index) (loop (+ index 1) array))])))

; TODO: list-of-seq
; TODO: list-pairwise
; TODO: list-partition - filter but keep true and false. (Pair (Listof T) (Listof T))
; TODO: list-permute - I have no idea how this works. Will have to see the source.

; TODO: Fix (: list-pick (All (T U) (-> (-> T (Option U)) (Listof T) (Option U))))

; TODO: list-remove-at

(define (split-at index source)
  (list (take index source) (skip index source)))

(define (skip count source)
  (when (> count (length source))
    (raise "Index is greater than the bounds of the list."))
  (let loop ([index 0] [count count] [source source])
    (cond
      [(null? source) '()]
      [(< index count) (loop (+ 1 index) count (cdr source))]
      [else (cons (car source) (loop (+ 1 index) count (cdr source)))])))

; (: list-split-into) Don't know how to determine how big each chunk should be.

(define (sum source)
  (fold (fn (acc item) (+ acc item)) 0 source))

; list-sum-by

(define (tail source)
  (cdr source))

(define (take count source)
  (when (> count (length source))
    (raise "Index is greater than the bounds of the list."))
  (let loop ([index 0] [count count] [source source])
    (cond
      [(= index count) '()]
      [else (cons (car source) (loop (+ 1 index) count (cdr source)))])))

(define (take-while predicate source)
  (cond
    [(null? source) '()]
    [(not (predicate (car source))) '()]
    [else (cons (car source) (take-while predicate (cdr source)))]))

; TODO: Change this to my own implementation.
(define (to-array source)
  (list->vector source))

; TODO: list->seq

; TODO: list->transpose

; TODO: list->truncate

; TODO: list-unfold, no idea how to implement

; TODO: list-unzip

; TODO: list-unzip-threeple

(define (sort source)
  (sort source <))

(define (sorty-by projection source)
  (sort source < #:key projection))

(define (sorty-by-descending projection source)
  (sort source > #:key projection))

(define (sort-descending source)
  (sort source >))

(define (update-at index value source)
  (let loop ([current 0] [index index] [value value] [source source])
    (cond
      [(null? source) '()]
      [(= current index) (cons value (loop (+ 1 current) index value (cdr source)))]
      [else (cons (car source) (loop (+ 1 current) index value (cdr source)))])))

; TODO: list-windowed

; TODO: list-zip - must be equal

; TODO: list-zip-three

(provide (all-defined-out))

(module+ test
  (require rackunit)
  (define names '("tom" "cindy" "billy"))

  (define (list-exists-two-test-pass)
    (define list-one '(1 2 3 4))
    (define list-two '(4 5 6 7))
    (check-true (exists-two (fn (x y) (= (+ x y) 5)) list-one list-two)))
  (list-exists-two-test-pass)

  (define (list-exists-two-test-fail)
    (define list-one '(1 2 3 4))
    (define list-two '(4 5 6 7))
    (check-false (exists-two (fn (x y) (= (+ x y) -1)) list-one list-two)))

  (define (list-filter-test-pass)
    (define source '("one" "two" "three"))
    (check-equal? (filter (fn (item) (< (string-length item) 5)) source) '("one" "two")))
  (list-filter-test-pass)

  (define (list-fold-two-test)
    (define result
      (fold-two (fn (state item-one item-two) (+ state item-one (string-length item-two)))
                0
                '(1 2 3)
                '("one" "two" "three")))

    (check-eq? result 17))
  (list-fold-two-test)

  (check-equal? (append (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))

  (check-eq? (average (list 0 0 1 4 5)) 2)

  (check-equal? (average-by (fn (x) (string-length x)) names) 13/3)

  (check-true (contains 3 '(1 2 3)))

  (check-false (contains "test" '("one")))

  (check-eq? (empty) '())

  (check-eq? (exactly-one '(1)) 1)

  (check-true (exists (fn (x) (> x 1)) '(1 2 3)))
  (check-false (exists (fn (x) (< x 1)) '(1 2 3)))

  (check-eq? (find (fn (item) (= item 5)) '(1 2 3 5 4)) 5)
  (check-false (find (fn (item) (= item 1)) '(2 3 4 5)))

  (check-eq? (find-back (fn (item) (= item 4)) '(1 2 3 5 4)) 4)
  (check-false (find-back (fn (item) (= item 0)) '(2 3 4 5)))

  (check-eq? (find-index (fn (item) (= item 3)) '(1 3 4 5)) 1)
  (check-false (find-index (fn (item) (equal? item "Test")) names))

  (check-eq? (find-index-back (fn (item) (= item 3)) '(1 3 4 5)) 1)
  (check-false (find-index-back (fn (item) (equal? item "Tim")) names))

  (check-eq? (fold (fn (state item) (+ state item)) 0 '(1 2 3)) 6)

  (check-equal? (fold (fn (state item) (string-append state " " item)) "" '("one" "two" "three"))
                " one two three")

  (check-equal? (fold-back (fn (state item) (/ item state)) 2 '(256 4048 24)) 192/253)
  (check-true (for-all (fn (item) (= 1 item)) '(1 1 1 1)))
  (check-false (for-all (fn (item) (= 1 item)) '()))

  (check-eq? (head '(1 2 3 4)) 1)

  (check-equal? (indexed '(1 2 3)) '((0 . 1) (1 . 2) (2 . 3)))

  (check-equal? (init 4
                      (fn (item) (+ item 5)))
                '(5 6 7 8))

  (check-eq? (item 1 '(1 2 3 4)) 2)
  (check-false (item 1 '()))
  (check-false (last '()))

  (check-eq? (last '(1 2 3 4 25 100)) 100)

  (check-eq? (length '(1 2 3 4)) 4)

  (check-equal? (map (fn (item) (+ item 2)) '(1 2 3 4)) '(3 4 5 6))

  (check-equal? (map-index (fn (index item) (+ index item)) '(1 2 3 4)) '(1 3 5 7))

  (check-eq? (max '(1 2 3 4)) 4)
  (check-equal? (max '(100.35 .33 .25 10.99)) 100.35)
  (check-equal? (of-array #(1 2 3 4)) '(1 2 3 4))
  (check-equal? (skip 1 '(1 2 3 4)) '(2 3 4))
  (check-equal? (take 2 '(1 2 3 4)) '(1 2))
  (check-equal? (split-at 3 '(8 4 3 1 6 1)) (list (list 8 4 3) (list 1 6 1)))
  (check-equal? (split-at 2 '(1 2 3 4 5)) (list (list 1 2) (list 3 4 5)))
  (check-eq? (sum '(1 2 3 4)) 10)
  (check-equal? (sum '(1.0 2.0 3.4 4.0)) 10.4)
  (check-equal? (take-while (fn (item) (not (= item 4))) '(1 2 3 4 5 6 7)) '(1 2 3))
  (check-equal? (update-at 1 3 '(1 2 3 4)) '(1 3 3 4))

  (check-equal? (sort '(4 2 3 1)) '(1 2 3 4))
  (check-equal? (sort-descending '(4 2 3 1)) '(4 3 2 1))

  (define unordered-pairs (list (list 4 "four") (list 2 "two") (list 3 "three")))

  (check-equal? (sorty-by (fn (x) (car x)) unordered-pairs)
                (list (list 2 "two") (list 3 "three") (list 4 "four")))
  (check-equal? (sorty-by-descending (fn (x) (car x)) unordered-pairs)
                (list (list 4 "four") (list 3 "three") (list 2 "two"))))
