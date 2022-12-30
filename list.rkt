; A library that contains all the functions for working with lists.
#lang typed/racket/base

(require "types.rkt")

(: list-all-pairs (All (T) (-> (Listof T) (Listof T) (Listof (List T T)))))
(define (list-all-pairs list-one list-two)
  (cond
    [(null? (cdr list-one)) (cons (list (car list-one) (car list-two)) '())]
    [else
     (cons (list (car list-one) (car list-two)) (list-all-pairs (cdr list-one) (cdr list-two)))]))

(: list-append (All (T) (-> (Listof T) (Listof T) (Listof T))))
(define (list-append list-one list-two)
  (append list-one list-two))

(: list-average (-> (Listof Number) Number))
(define (list-average source)
  (/ (apply + source) (length source)))

(: list-average-by (All (T) (-> (-> T Number) (Listof T) Number)))
(define (list-average-by projection source)
  (list-average (map projection source)))

(: list-choose (All (T U) (-> (-> T (Option U)) (Listof T) (Listof (Option U)))))
(define (list-choose chooser source)
  (raise "Not implemented."))

(: list-chunk-by-size (All (T) (-> Integer (Listof T) (Listof (Listof T)))))
(define (list-chunk-by-size chunk-size source)
  (raise "Not implemented."))

(: list-collect (All (T U) (-> (-> T (Listof U)) (Listof T) (Listof U))))
(define (list-collect mapping source)
  (raise "Not implemented"))

(: list-compare-with (All (T) (-> (-> T T Integer) (Listof T) (Listof T) Integer)))
(define (list-compare-with comparer list-one list-two)
  ;;; TODO: Implement a compare function.
  (raise "Not implemented"))

(: list-concat (All (T) (-> (Sequenceof (Listof T)) (Listof T))))
(define (list-concat lists)
  (raise "Not implemented"))

(: list-contains (All (T) (-> T (Listof T) Boolean)))
(define (list-contains value source)
  (cond
    [(null? source) #f]
    [(equal? (car source) value) #t]
    [else (list-contains value (cdr source))]))

(: list-count-by (All (T U) (-> (-> T U) (Listof T) (List U T))))
(define (list-count-by projection source)
  (raise "Not implemented."))

(: list-distinct (All (T) (-> (Listof T) (Listof T))))
(define (list-distinct source)
  (raise "Not implemented."))

(: list-distinct-by (All (T U) (-> (-> T U) (Listof T) (Listof T))))
(define (list-distinct-by projection source)
  (raise "Not implemented."))

(: list-empty (All (T) (-> (Listof T))))
(define (list-empty)
  '())

(: list-exactly-one (All (T) (-> (List T) T)))
(define (list-exactly-one source)
  (car source))

; TODO: Fix this after I create a seq-contains function.
;(: list-except (All (T) (-> (Sequenceof T) (Listof T) (Listof T))))
;(define #:forall (T) (list-except items-to-exclude source)
;  (filter (lambda ([item : T]) (contains item items-to-exclude)) source))

(: list-exists (All (T) (-> (-> T Boolean) (Listof T) Boolean)))
(define (list-exists predicate source)
  (cond
    [(null? source) #f]
    [(predicate (car source)) #t]
    [else (list-exists predicate (cdr source))]))

(: list-exists-two (All (T) (-> (-> T T Boolean) (Listof T) (Listof T) Boolean)))
(define (list-exists-two predicate list-one list-two)
  (cond
    [(and (null? list-one) (null? list-two)) #f]
    [(predicate (car list-one) (car list-two)) #t]
    [(null? (cdr list-one)) (list-exists-two predicate list-one (cdr list-two))]
    [else (list-exists-two predicate (cdr list-one) list-two)]))

; TODO: Build this myself. It will be better because it's typed, no contract.
(: list-filter (All (T) (-> (-> T Boolean) (Listof T) (Listof T))))
(define (list-filter predicate source)
  (filter predicate source))

(: list-find (All (T) (-> (-> T Boolean) (Listof T) (Option T))))
(define (list-find predicate source)
  (cond
    [(null? source) #f]
    [(predicate (car source)) (car source)]
    [else (list-find predicate (cdr source))]))

(: list-find-back (All (T) (-> (-> T Boolean) (Listof T) (Option T))))
(define (list-find-back predicate source)
  (define reverse-source (reverse source))
  (cond
    [(null? reverse-source) #f]
    [(predicate (car reverse-source)) (car reverse-source)]
    [else (list-find predicate (cdr reverse-source))]))

(: list-find-index (All (T) (-> (-> T Boolean) (Listof T) (Option Integer))))
(define (list-find-index predicate source)
  (let loop ([index 0] [predicate predicate] [source source])
    (cond
      [(null? source) #f]
      [(predicate (car source)) index]
      [else (loop (+ index 1) predicate (cdr source))])))

(: list-find-index-back (All (T) (-> (-> T Boolean) (Listof T) (Option Integer))))
(define (list-find-index-back predicate source)
  (define reverse-source (reverse source))
  (define count (- (length source) 1))

  (let loop ([index count] [predicate predicate] [source reverse-source])
    (cond
      [(null? source) #f]
      [(predicate (car source)) index]
      [else (loop (- index 1) predicate (cdr source))])))

(: list-fold (All (T U) (-> (-> U T U) U (Listof T) U)))
(define (list-fold folder state source)
  (cond
    [(null? source) state]
    [else (list-fold folder (folder state (car source)) (cdr source))]))

(: list-fold-two (All (T U V) (-> (-> U T V U) U (Listof T) (Listof V) U)))
(define (list-fold-two folder state list-one list-two)
  (cond
    [(and (null? list-one) (null? list-two)) state]
    [(and (null? (cdr list-one)) (not (null? (cdr list-two))))
     (list-fold-two folder (folder state (car list-one) (car list-two)) list-one (cdr list-two))]
    [(and (null? (cdr list-two)) (not (null? (cdr list-one))))
     (list-fold-two folder (folder state (car list-one) (car list-two)) (cdr list-one) list-two)]
    [else
     (list-fold-two folder
                    (folder state (car list-one) (car list-two))
                    (cdr list-one)
                    (cdr list-two))]))

(: list-fold-back (All (T U) (-> (-> U T U) U (Listof T) U)))
(define (list-fold-back folder state source)
  (define reverse-source (reverse source))
  (list-fold folder state reverse-source))

(: list-fold-back-two (All (T U V) (-> (-> U T V U) U (Listof T) (Listof V) U)))
(define (list-fold-back-two folder state list-one list-two)
  (define list-one-reverse (reverse list-one))
  (define list-two-reverse (reverse list-two))

  (cond
    [(and (null? list-one-reverse) (null? list-two-reverse)) state]
    [(and (null? (cdr list-one-reverse)) (not (null? (cdr list-two-reverse))))
     (list-fold-two folder
                    (folder state (car list-one-reverse) (car list-two-reverse))
                    list-one-reverse
                    (cdr list-two-reverse))]
    [(and (null? (cdr list-two-reverse)) (not (null? (cdr list-one-reverse))))
     (list-fold-two folder
                    (folder state (car list-one-reverse) (car list-two-reverse))
                    (cdr list-one-reverse)
                    list-two-reverse)]
    [else
     (list-fold-two folder
                    (folder state (car list-one-reverse) (car list-two-reverse))
                    (cdr list-one-reverse)
                    (cdr list-two-reverse))]))

(: list-for-all (All (T) (-> (-> T Boolean) (Listof T) Boolean)))
(define (list-for-all predicate source)
  (if (> (length (list-filter predicate source)) 0) #t #f))

(: list-for-all-two (All (T) (-> (-> T T Boolean) (Listof T) (Listof T) Boolean)))
(define (list-for-all-two predicate list-one list-two)
  (raise "Not implemented."))

(: list-group-by (All (T Key) (-> (-> T Key) (Listof T) (Listof (Pair Key T)))))
(define (list-group-by projection source)
  (raise "Not implemented."))

(: list-head (All (T) (-> (Listof T) T)))
(define (list-head source)
  (car source))

(: list-indexed (All (T) (-> (Listof T) (Listof (Pair Integer T)))))
(define (list-indexed source)
  (: loop (All (T) (-> Integer (Listof T) (Listof (Pair Integer T)))))
  (define (loop index source)
    (cond
      [(null? source) '()]
      [(append (list (cons index (car source))) (loop (+ 1 index) (cdr source)))]))
  (loop 0 source))

(: list-init (All (T) (-> Integer (-> Integer T) (Listof T))))
(define (list-init count initializer)
  (: loop (All (T) (-> Integer Integer (-> Integer T) (Listof T))))
  (define (loop index count initializer)
    (cond
      [(= index count) '()]
      [else (cons (initializer index) (loop (+ 1 index) count initializer))]))
  (loop 0 count initializer))

; TODO: Build a splitter.
(: list-insert (All (T) (-> u32 T (Listof T) (Listof T))))
(define (list-insert index value source)
  (raise "Not implemented."))

; list-insert-many-at

(: list-is-empty (All (T) (-> (Listof T) Boolean)))
(define (list-is-empty source)
  (null? source))

(: list-item (All (T) (-> u32 (Listof T) T)))
(define (list-item index source)
  (let loop ([acc 0] [index index] [source source])
    (cond
      [(= acc index) (car source)]
      [else (loop (+ 1 acc) index (cdr source))])))

(provide (all-defined-out))

;; Tests
(module+ test
  (require typed/rackunit)
  (define names '("tom" "cindy" "billy"))

  (define (list-exists-two-test-pass)
    (: list-one (Listof Number))
    (define list-one '(1 2 3 4))
    (: list-two (Listof Number))
    (define list-two '(4 5 6 7))
    (check-true
     (list-exists-two (lambda ([x : Number] [y : Number]) (= (+ x y) 5)) list-one list-two)))
  (list-exists-two-test-pass)

  ;TODO Fix.
  (define (list-exists-two-test-fail)
    (: list-one (Listof Number))
    (define list-one '(1 2 3 4))
    (: list-two (Listof Number))
    (define list-two '(4 5 6 7))
    (check-false
     (list-exists-two (lambda ([x : Number] [y : Number]) (= (+ x y) -1)) list-one list-two)))

  (define (list-filter-test-pass)
    (: source (Listof String))
    (define source '("one" "two" "three"))
    (check-equal? (list-filter (lambda ([item : String]) (< (string-length item) 5)) source)
                  '("one" "two")))
  (list-filter-test-pass)

  (define (list-fold-two-test)
    (define result
      (list-fold-two (lambda ([state : Integer] [item-one : Integer] [item-two : String])
                       (+ state item-one (string-length item-two)))
                     0
                     '(1 2 3)
                     '("one" "two" "three")))

    (check-eq? result 17))
  (list-fold-two-test)

  (check-equal? (list-append (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))

  (check-eq? (list-average (list 0 0 1 4 5)) 2)

  (check-equal? (list-average-by (lambda ([x : String]) (string-length x)) names) 13/3)

  (check-true (list-contains 3 '(1 2 3)))

  (check-false (list-contains "test" '("one")))

  (check-eq? (list-empty) '())

  (check-eq? (list-exactly-one '(1)) 1)

  (check-true (list-exists (lambda ([x : Integer]) (> x 1)) '(1 2 3)))
  (check-false (list-exists (lambda ([x : Integer]) (< x 1)) '(1 2 3)))

  (check-eq? (list-find (lambda ([item : Integer]) (= item 5)) '(1 2 3 5 4)) 5)
  (check-false (list-find (lambda ([item : Integer]) (= item 1)) '(2 3 4 5)))

  (check-eq? (list-find-back (lambda ([item : Integer]) (= item 4)) '(1 2 3 5 4)) 4)
  (check-false (list-find-back (lambda ([item : Integer]) (= item 0)) '(2 3 4 5)))

  (check-eq? (list-find-index (lambda ([item : Integer]) (= item 3)) '(1 3 4 5)) 1)
  (check-false (list-find-index (lambda ([item : String]) (equal? item "Test")) names))

  (check-eq? (list-find-index-back (lambda ([item : Integer]) (= item 3)) '(1 3 4 5)) 1)
  (check-false (list-find-index-back (lambda ([item : String]) (equal? item "Tim")) names))

  (check-eq? (list-fold (lambda ([state : Integer] [item : Integer]) (+ state item)) 0 '(1 2 3)) 6)

  (check-equal? (list-fold (lambda ([state : String] [item : String]) (string-append state " " item))
                           ""
                           '("one" "two" "three"))
                " one two three")

  (check-equal?
   (list-fold-back (lambda ([state : Number] [item : Number]) (/ item state)) 2 '(256 4048 24))
   192/253)
  (check-true (list-for-all (lambda ([item : Integer]) (= 1 item)) '(1 1 1 1)))
  (check-false (list-for-all (lambda ([item : Integer]) (= 1 item)) '()))
  ;(check-equal? (list-group-by (lambda ([item : Integer]) (modulo item 2)) '(1 2 3 4 5))
  ;'((list 1 '(1 3 5)) (list 0 '(2 4))))

  (check-eq? (list-head '(1 2 3 4)) 1)

  (check-equal? (list-indexed '(1 2 3)) '((0 . 1) (1 . 2) (2 . 3)))

  (check-equal? (list-init 4 (lambda ([item : Integer]) (+ item 5))) '(5 6 7 8))

  (check-eq? (list-item 1 '(1 2 3 4)) 2))
