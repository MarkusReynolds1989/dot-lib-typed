#lang typed/racket/base

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

(module+ test
  (require typed/rackunit)
  (define names '("tom" "cindy" "billy"))

  (check-equal? (list-append (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
  (check-equal? (list-average (list 0 0 1 4 5)) 2)
  (check-equal? (list-average-by (lambda ([x : String]) (string-length x)) names) 13/3)
  (check-true (list-contains 3 '(1 2 3)))
  (check-false (list-contains "test" '("one"))))
