#lang typed/racket/base

(: list-append (All (T) (-> (Listof T) (Listof T) (Listof T))))
(define (list-append first-list second-list)
  (append first-list second-list))

(: list-average (All (T) (-> (Listof Number) Number)))
(define (list-average input)
  (/ (apply + input) (length input)))

(: list-choose (All (T U) (-> (-> T (Option U)) (Listof T) (Listof U))))
(define (list-choose chooser input)
  ())

(module+ test
  (require typed/rackunit)
  (check-equal? (list-append (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
  (check-equal? (list-average (list 0 0 1 4 5)) 2))
