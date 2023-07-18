; A library that contains all the functions for working with lists.
; Instead of having "try" functions that produce option and regular functions that raise exceptions
; I am defaulting to optional functions.

#lang racket/base

(define (List-all-pairs list-one list-two)
  (cond
    [(null? (cdr list-one)) (cons (list (car list-one) (car list-two)) '())]
    [else
     (cons (list (car list-one) (car list-two)) (List-all-pairs (cdr list-one) (cdr list-two)))]))

(define (List-append list-one list-two)
  (append list-one list-two))

(define (List-average source)
  (/ (apply + source) (List-length source)))

(define (List-average-by projection source)
  (List-average (map projection source)))

(define (List-choose chooser source)
  (raise "Not implemented."))

(define (List-chunk-by-size chunk-size source)
  (raise "Not implemented."))

(define (List-collect mapping source)
  (raise "Not implemented"))

(define (List-compare-with comparer list-one list-two)
  ;;; TODO: Implement a compare function.
  (raise "Not implemented"))

(define (List-concat lists)
  (raise "Not implemented"))

(define (List-contains value source)
  (cond
    [(null? source) #f]
    [(equal? (car source) value) #t]
    [else (List-contains value (cdr source))]))

(define (List-count-by projection source)
  (raise "Not implemented."))

(define (List-distinct source)
  (raise "Not implemented."))

(define (List-distinct-by projection source)
  (raise "Not implemented."))

(define (List-empty)
  '())

(define (List-exactly-one source)
  (car source))

;(define List-forall (T) (list-except items-to-exclude source)
;  (filter (lambda ([item : T]) (contains item items-to-exclude)) source))

(define (List-exists predicate source)
  (cond
    [(null? source) #f]
    [(predicate (car source)) #t]
    [else (List-exists predicate (cdr source))]))

(define (List-exists-two predicate list-one list-two)
  (cond
    [(and (null? list-one) (null? list-two)) #f]
    [(predicate (car list-one) (car list-two)) #t]
    [(null? (cdr list-one)) (List-exists-two predicate list-one (cdr list-two))]
    [else (List-exists-two predicate (cdr list-one) list-two)]))

(define (List-filter predicate source)
  (filter predicate source))

(define (List-find predicate source)
  (cond
    [(null? source) #f]
    [(predicate (car source)) (car source)]
    [else (List-find predicate (cdr source))]))

(define (List-find-back predicate source)
  (define reverse-source (reverse source))
  (cond
    [(null? reverse-source) #f]
    [(predicate (car reverse-source)) (car reverse-source)]
    [else (List-find predicate (cdr reverse-source))]))

(define (List-find-index predicate source)
  (let loop ([index 0] [predicate predicate] [source source])
    (cond
      [(null? source) #f]
      [(predicate (car source)) index]
      [else (loop (+ index 1) predicate (cdr source))])))

(define (List-find-index-back predicate source)
  (define reverse-source (reverse source))
  (define count (- (List-length source) 1))

  (let loop ([index count] [predicate predicate] [source reverse-source])
    (cond
      [(null? source) #f]
      [(predicate (car source)) index]
      [else (loop (- index 1) predicate (cdr source))])))

(define (List-fold folder state source)
  (cond
    [(null? source) state]
    [else (List-fold folder (folder state (car source)) (cdr source))]))

(define (List-fold-two folder state list-one list-two)
  (cond
    [(and (null? list-one) (null? list-two)) state]
    [(and (null? (cdr list-one)) (not (null? (cdr list-two))))
     (List-fold-two folder (folder state (car list-one) (car list-two)) list-one (cdr list-two))]
    [(and (null? (cdr list-two)) (not (null? (cdr list-one))))
     (List-fold-two folder (folder state (car list-one) (car list-two)) (cdr list-one) list-two)]
    [else
     (List-fold-two folder
                    (folder state (car list-one) (car list-two))
                    (cdr list-one)
                    (cdr list-two))]))

(define (List-fold-back folder state source)
  (define reverse-source (reverse source))
  (List-fold folder state reverse-source))

(define (List-fold-back-two folder state list-one list-two)
  (define list-one-reverse (reverse list-one))
  (define list-two-reverse (reverse list-two))

  (cond
    [(and (null? list-one-reverse) (null? list-two-reverse)) state]
    [(and (null? (cdr list-one-reverse)) (not (null? (cdr list-two-reverse))))
     (List-fold-two folder
                    (folder state (car list-one-reverse) (car list-two-reverse))
                    list-one-reverse
                    (cdr list-two-reverse))]
    [(and (null? (cdr list-two-reverse)) (not (null? (cdr list-one-reverse))))
     (List-fold-two folder
                    (folder state (car list-one-reverse) (car list-two-reverse))
                    (cdr list-one-reverse)
                    list-two-reverse)]
    [else
     (List-fold-two folder
                    (folder state (car list-one-reverse) (car list-two-reverse))
                    (cdr list-one-reverse)
                    (cdr list-two-reverse))]))

(define (List-for-all predicate source)
  (if (> (List-length (List-filter predicate source)) 0) #t #f))

(define (List-for-all-two predicate list-one list-two)
  (raise "Not implemented."))

(define (List-group-by projection source)
  (raise "Not implemented."))

(define (List-head source)
  (if (null? source) #f (car source)))

(define (List-indexed source)
  (let loop ([index 0] [source source])
    (cond
      [(null? source) '()]
      [(append (list (cons index (car source))) (loop (+ 1 index) (cdr source)))])))

(define (List-init count initializer)
  (let loop ([index 0] [count count] [initializer initializer])
    (cond
      [(= index count) '()]
      [else (cons (initializer index) (loop (+ 1 index) count initializer))])))

; TODO: Build a splitter.
(define (List-insert index value source)
  (raise "Not implemented."))

; TODO: list-insert-many-at

(define (List-is-empty? source)
  (null? source))

(define (List-item index source)
  (let loop ([acc 0] [index index] [source source])
    (cond
      [(null? source) #f]
      [(= acc index) (car source)]
      [else (loop (+ 1 acc) index (cdr source))])))

(define (List-iter action source)
  (cond
    [(null? source) (void)]
    [else
     (action (car source))
     (List-iter action (cdr source))]))

; TODO: Finish all cases and test.
(define (List-iter-two action list-one list-two)
  (cond
    [(and (null? list-one) (null? list-two)) (void)]
    [(and (null? (cdr list-two)) (not (null? (cdr list-one))))
     (action (car list-one) (car list-two))
     (List-iter-two action (cdr list-one) list-two)]))

(define (List-iter-index action source)
  (let loop ([index 0] [action action] [source source])
    (cond
      [(null? source) (void)]
      [else
       (action index (car source))
       (loop (+ 1 index) action (cdr source))])))

(define (List-iter-index-two action list-one list-two)
  (when (not (= (List-length list-one) (List-length list-two)))
    (raise "Arguments must be the same length to list-iter-index-two."))
  (let loop ([index 0] [action action] [list-one list-one] [list-two list-two])
    (cond
      [(null? list-one) (void)]
      [else
       (action index (car list-one) (car list-two))
       (loop (+ 1 index) action (cdr list-one) (cdr list-two))])))

(define (List-last source)
  (if (null? source) #f (List-item (- (List-length source) 1) source)))

(define (List-length source)
  (if (null? source) 0 (+ 1 (List-length (cdr source)))))

(define (List-map mapping source)
  (cond
    [(null? source) '()]
    [else (cons (mapping (car source)) (List-map mapping (cdr source)))]))

; TODO: list-map-two
; TODO: list-map-three
; TODO: list-map-fold - returns the list and accumulated value as a pair.
; TODO: list-map-fold-back - same as above but with a reversed list.

(define (List-map-index mapping source)
  (let loop ([index 0] [mapping mapping] [source source])
    (cond
      [(null? source) '()]
      [else (cons (mapping index (car source)) (loop (+ 1 index) mapping (cdr source)))])))

; TODO: list-map-index-two - Lists same length.

; TODO: Come back to fix the wonky type checking.
(define (List-max source)
  (List-fold (lambda (max-num item) (if (< max-num item) item max-num)) -320000 source))

; TODO: list-max-by
; TODO: list-min
; TODO: list-min-by

(define (List-of-array array)
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

(define (List-split-at index source)
  (list (List-take index source) (List-skip index source)))

(define (List-skip count source)
  (when (> count (List-length source))
    (raise "Index is greater than the bounds of the list."))
  (let loop ([index 0] [count count] [source source])
    (cond
      [(null? source) '()]
      [(< index count) (loop (+ 1 index) count (cdr source))]
      [else (cons (car source) (loop (+ 1 index) count (cdr source)))])))

; (: list-split-into) Don't know how to determine how big each chunk should be.

(define (List-sum source)
  (List-fold (lambda (acc item) (+ acc item)) 0 source))

; list-sum-by

(define (List-tail source)
  (cdr source))

(define (List-take count source)
  (when (> count (List-length source))
    (raise "Index is greater than the bounds of the list."))
  (let loop ([index 0] [count count] [source source])
    (cond
      [(= index count) '()]
      [else (cons (car source) (loop (+ 1 index) count (cdr source)))])))

(define (List-take-while predicate source)
  (cond
    [(null? source) '()]
    [(not (predicate (car source))) '()]
    [else (cons (car source) (List-take-while predicate (cdr source)))]))

; TODO: Change this to my own implementation.
(define (List->array source)
  (list->vector source))

; TODO: list->seq

; TODO: list->transpose

; TODO: list->truncate

; TODO: list-unfold, no idea how to implement

; TODO: list-unzip

; TODO: list-unzip-threeple

(define (List-sort source)
  (sort source <))

(define (List-sort-by projection source)
  (sort source < #:key projection))

(define (List-sort-by-descending projection source)
  (sort source > #:key projection))

(define (List-sort-descending source)
  (sort source >))

(define (List-update-at index value source)
  (let loop ([current 0] [index index] [value value] [source source])
    (cond
      [(null? source) '()]
      [(= current index) (cons value (loop (+ 1 current) index value (cdr source)))]
      [else (cons (car source) (loop (+ 1 current) index value (cdr source)))])))

; TODO: list-windowed

; TODO: list-zip - must be equal

; TODO: list-zip-three

(provide (all-defined-out))

;; Tests
(module+ test
  (require rackunit)
  (define names '("tom" "cindy" "billy"))

  (define (list-exists-two-test-pass)
    (define list-one '(1 2 3 4))
    (define list-two '(4 5 6 7))
    (check-true (List-exists-two (lambda (x y) (= (+ x y) 5)) list-one list-two)))
  (list-exists-two-test-pass)

  ;TODO Fix.
  (define (list-exists-two-test-fail)
    (define list-one '(1 2 3 4))
    (define list-two '(4 5 6 7))
    (check-false (List-exists-two (lambda (x y) (= (+ x y) -1)) list-one list-two)))

  (define (list-filter-test-pass)
    (define source '("one" "two" "three"))
    (check-equal? (List-filter (lambda (item) (< (string-length item) 5)) source) '("one" "two")))
  (list-filter-test-pass)

  (define (list-fold-two-test)
    (define result
      (List-fold-two (lambda (state item-one item-two) (+ state item-one (string-length item-two)))
                     0
                     '(1 2 3)
                     '("one" "two" "three")))

    (check-eq? result 17))
  (list-fold-two-test)

  (check-equal? (List-append (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))

  (check-eq? (List-average (list 0 0 1 4 5)) 2)

  (check-equal? (List-average-by (lambda (x) (string-length x)) names) 13/3)

  (check-true (List-contains 3 '(1 2 3)))

  (check-false (List-contains "test" '("one")))

  (check-eq? (List-empty) '())

  (check-eq? (List-exactly-one '(1)) 1)

  (check-true (List-exists (lambda (x) (> x 1)) '(1 2 3)))
  (check-false (List-exists (lambda (x) (< x 1)) '(1 2 3)))

  (check-eq? (List-find (lambda (item) (= item 5)) '(1 2 3 5 4)) 5)
  (check-false (List-find (lambda (item) (= item 1)) '(2 3 4 5)))

  (check-eq? (List-find-back (lambda (item) (= item 4)) '(1 2 3 5 4)) 4)
  (check-false (List-find-back (lambda (item) (= item 0)) '(2 3 4 5)))

  (check-eq? (List-find-index (lambda (item) (= item 3)) '(1 3 4 5)) 1)
  (check-false (List-find-index (lambda (item) (equal? item "Test")) names))

  (check-eq? (List-find-index-back (lambda (item) (= item 3)) '(1 3 4 5)) 1)
  (check-false (List-find-index-back (lambda (item) (equal? item "Tim")) names))

  (check-eq? (List-fold (lambda (state item) (+ state item)) 0 '(1 2 3)) 6)

  (check-equal?
   (List-fold (lambda (state item) (string-append state " " item)) "" '("one" "two" "three"))
   " one two three")

  (check-equal? (List-fold-back (lambda (state item) (/ item state)) 2 '(256 4048 24)) 192/253)
  (check-true (List-for-all (lambda (item) (= 1 item)) '(1 1 1 1)))
  (check-false (List-for-all (lambda (item) (= 1 item)) '()))
  ;(check-equal? (list-group-by (lambda ([item : Integer]) (modulo item 2)) '(1 2 3 4 5))
  ;'((list 1 '(1 3 5)) (list 0 '(2 4))))

  (check-eq? (List-head '(1 2 3 4)) 1)

  (check-equal? (List-indexed '(1 2 3)) '((0 . 1) (1 . 2) (2 . 3)))

  (check-equal? (List-init 4 (lambda (item) (+ item 5))) '(5 6 7 8))

  (check-eq? (List-item 1 '(1 2 3 4)) 2)
  (check-false (List-item 1 '()))
  (check-false (List-last '()))

  (check-eq? (List-last '(1 2 3 4 25 100)) 100)

  (check-eq? (List-length '(1 2 3 4)) 4)

  (check-equal? (List-map (lambda (item) (+ item 2)) '(1 2 3 4)) '(3 4 5 6))

  (check-equal? (List-map-index (lambda (index item) (+ index item)) '(1 2 3 4)) '(1 3 5 7))

  (check-eq? (List-max '(1 2 3 4)) 4)
  (check-equal? (List-max '(100.35 .33 .25 10.99)) 100.35)
  (check-equal? (List-of-array #(1 2 3 4)) '(1 2 3 4))
  (check-equal? (List-skip 1 '(1 2 3 4)) '(2 3 4))
  (check-equal? (List-take 2 '(1 2 3 4)) '(1 2))
  (check-equal? (List-split-at 3 '(8 4 3 1 6 1)) (list (list 8 4 3) (list 1 6 1)))
  (check-equal? (List-split-at 2 '(1 2 3 4 5)) (list (list 1 2) (list 3 4 5)))
  (check-eq? (List-sum '(1 2 3 4)) 10)
  (check-equal? (List-sum '(1.0 2.0 3.4 4.0)) 10.4)
  (check-equal? (List-take-while (lambda (item) (not (= item 4))) '(1 2 3 4 5 6 7)) '(1 2 3))
  (check-equal? (List->array '(1 2 3 4)) #(1 2 3 4))
  (check-equal? (List-update-at 1 3 '(1 2 3 4)) '(1 3 3 4))

  (check-equal? (List-sort '(4 2 3 1)) '(1 2 3 4))
  (check-equal? (List-sort-descending '(4 2 3 1)) '(4 3 2 1))

  (define unordered-pairs (list (list 4 "four") (list 2 "two") (list 3 "three")))

  (check-equal? (List-sort-by (lambda (x) (car x)) unordered-pairs) (list (list 2 "two") (list 3 "three") (list 4 "four")))
  (check-equal? (List-sort-by-descending (lambda (x) (car x)) unordered-pairs) (list (list 4 "four") (list 3 "three") (list 2 "two"))))
