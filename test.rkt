#lang typed/racket/base

(require racket/file
         "list.rkt")

(define test-data
  (list "1000" "2000" "3000" "" "4000" "" "5000" "6000" "" "7000" "8000" "9000" "10000"))

(define filePath "day1.txt")

(define lines (file->lines filePath))

(: unwrap-or (All (T) (-> T (U T False) T)))
(define (unwrap-or default value)
  (if (equal? value #f) default value))

(: manage-calories
   (-> (List (Listof Number) (Listof Number)) String (List (Listof Number) (Listof Number))))
(define (manage-calories acc index)
  (if (<= (string-length index) 0)
      (list (list) (cons (list-sum (car acc)) (car (cdr acc))))
      (list (cons (unwrap-or 0 (string->number index)) (car acc)) (car (cdr acc)))))

(list-max (list-map (lambda ([item : Number]) (cast item Real))
                    (car (cdr (list-fold manage-calories (list (list) (list)) lines)))))

;(car (cdr (list-fold manage-calories (list (list) (list)) lines)))
