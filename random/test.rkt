#lang typed/racket/base

(require racket/file
         threading
         "../utils/option.rkt"
         "../utils/tuple.rkt"
         "../collections/list.rkt")

(define test-data
  (list "1000" "2000" "3000" "" "4000" "" "5000" "6000" "" "7000" "8000" "9000" "10000"))

(define filePath "random/day1.txt")

(define lines (file->lines filePath))

(: manage-calories
   (-> (Tuple (Listof Number) (Listof Number)) String (Tuple (Listof Number) (Listof Number))))
(define (manage-calories acc index)
  (if (<= (string-length index) 0)
      (list (list-empty) (cons (list-sum (tuple-frst acc)) (car (tuple-snd acc))))
      (list (cons (unwrap-or 0 (string->number index)) (tuple-frst acc)) (car (tuple-snd acc)))))

(~> (car (cdr (list-fold manage-calories (list (list) (list)) lines)))
    (list-map (lambda ([item : Number]) (cast item Real)) _)
    (list-max))

;(car (cdr (list-fold manage-calories (list (list) (list)) lines)))
