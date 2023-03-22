#lang racket/base

(require "list.rkt")

(define (Map-add key value table)
  (hash-set table key value))

(define (Map-change key f table)
  (hash-update table key f))

(define (Map-contains-key key table)
  (hash-has-key? table key))

(define (Map-count table)
  (hash-count table))

(define (Map-empty)
  (hash))

(define (Map-exists predicate table)
  (> (Map-count (Map-filter predicate table)) 0))

(define (Map-map mapping table)
  (hash-map/copy table mapping))

(define (filter predicate acc pair)
  (if (predicate pair) (Map-add (car pair) (cdr pair) acc) acc))

(define (Map-filter predicate table)
  (Map-fold (lambda (acc pair) (filter predicate acc pair)) (Map-empty) table))

(define (Map-find key table)
 (hash-ref table key))

(define (Map-find-key predicate table)
  ( ))

(define (Map-fold folder state table)
  (let loop ([index 0] [folder folder] [state state] [table table])
    (cond
      [(= index (Map-count table)) state]
      [#t (loop (add1 index) folder (folder state (hash-iterate-pair table index)) table)])))

(define (Map->list table)
  (hash-map table (lambda (key value) (list key value))))

(define (Map->array table)
  (List->array (hash-map table (lambda (key value) (list key value)))))

(provide (all-defined-out) (except-out filter))

(module+ test
  (require rackunit)
  (define table (hash 1 "one"))
  (define add-table (hash "one" 1 "two" 2 "three" 3))
  (check-equal? (Map-add 2 "two" table) (hash 1 "one" 2 "two"))
  (check-equal? (Map-change 1 (lambda (_) "three") table) (hash 1 "three"))
  (check-true (Map-contains-key 1 table))
  (check-eq? (Map-count table) 1)
  (check-equal? (Map-empty) (hash))
  (check-equal? (Map->list table) (list (list 1 "one")))
  (check-equal? (Map->array table) (vector (list 1 "one")))
  (check-equal? (Map-map (lambda (key value) (values (add1 key) value)) table) (hash 2 "one"))
  (check-equal? (Map-fold (lambda (acc pair) (+ (cdr pair) acc)) 0 add-table) 6)
  (check-equal? (Map-filter (lambda (pair) (> (cdr pair) 1)) add-table) (hash "two" 2 "three" 3))
  (check-true (Map-exists (lambda (x) (equal? (car x) "one")) add-table))
  (check-equal? (Map-find "one" add-table) 1))
