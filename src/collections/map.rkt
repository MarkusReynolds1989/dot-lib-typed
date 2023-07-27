#lang typed/racket/base/deep

(require "../globals.rkt")

; Returns a new map witht the binding added to the given map. If a binding with the given
; key already exists in the input map, the existing binding is replaced by the new binding
; in the result map.
(: add (All (Key T) (-> Key T (HashTable Key T) (HashTable Key T))))
(define (add key value table)
  (hash-set table key value))

; change
; (: change (All (Key)))

; Tests if an element is in the domain of the map.
(: contains-key (All (Key T) (-> Key (HashTable Key T) Boolean)))
(define (contains-key key table)
  (hash-has-key? table key))

; The number of bindings in the map.
(: count (All (Key T) (-> (HashTable Key T) Integer)))
(define (count table)
  (hash-count table))

; The empty map.
(: empty (All (Key T) -> (HashTable Key T)))
(define (empty)
  (ann (hash) (HashTable Key T)))

; Returns true if the given predicat returns true for one of the bindings in the map.
;(: exists (All (Key T) (-> (-> Key T Boolean) (HashTable Key T) Boolean)))
;(define (exists predicate table)
;  (> (count (filter predicate table)) 0))

; Get the element at the key.
(: get (All (Key T) (-> Key (HashTable Key T) T)))
(define (get key table)
  (hash-ref table key))

; The keys in the map. THe sequence will be ordered by the keys of the map.
(: keys (All (Key T) (-> (HashTable Key T) (Listof Key))))
(define (keys table)
  (hash-keys table))

; Builds a new collection whose elements are the results of applying the given
; function to each of the elements of the collection. The key passed to the function
; indicates the key of elements being transformed.
; (: map (All (Key T S) (-> (-> T S) (HashTable Key T) (HashTable Key S))))
; (define (map mapping table)
;  (for/hash ([i (keys table)])
;    (values i (mapping (get table i)))))

; Private function only used in the filter function to follow.
;(: map-filter (All (Key T) (-> (-> Key T Boolean) T (Pairof Key T))))
;(define (map-filter predicate acc pair)
;  (if (predicate pair) (add (car pair) (cdr pair) acc) acc))

; Builds a new map containing only the bindings for which the given predicate returns true.
;(: filter (All (Key T) (-> (-> Key T Boolean) (HashTable Key T) (HashTable Key T))))
;(define (filter predicate table)
;  (fold (fn (acc pair) (map-filter predicate acc pair)) (empty) table))

; Lookup an element in the map, raising KeyNotFoundException if no binding exists in the map.
(: find (All (Key T) (-> Key (HashTable Key T) T)))
(define (find key table)
  (hash-ref table key))

; Folds over the bindings in the map.
(: fold (All (State Key T) (-> (-> State Key T State) State (HashTable Key T) State)))
(define (fold folder init-state table)
  (let loop ([index 0] [folder folder] [state init-state] [table table])
    (cond
      [(= index (count table)) state]
      [else
       (loop (+ index 1)
             folder
             (folder state (hash-iterate-key table index) (hash-iterate-value table index))
             table)])))

(: to-list (All (Key T) (-> (HashTable Key T) (Listof (Tuple Key T)))))
(define (to-list table)
  (for/list ([i (keys table)])
    (Tuple i (get i table))))

;(define (to-array table)
;  (List.to-array (hash-map table (fn (key value) (list key value)))))

(: get-values (All (Key T) (-> (HashTable Key T) (Listof T))))
(define (get-values table)
  (hash-values table))

(provide (all-defined-out))

(module+ test
  (require typed/rackunit)
  (define table (hash 1 "one"))
  (define add-table (hash "one" 1 "two" 2 "three" 3))

  (test-equal? "Add works." (add 2 "two" table) (hash 1 "one" 2 "two"))

  (test-true "Contains works." (contains-key 1 table))

  (test-eq? "Count works." (count table) 1)

  (test-eq? "Get works." (get 1 table) "one")

  (test-equal? "Empty works." (empty) (hash))

  ;(test-equal? "To-list works." (to-list table) (list (list 1 "one")))

  ;(test-equal? "To-array works." (to-array table) (vector (list 1 "one")))

  ;(test-equal? "Map works." (map (fn (key value) (values (add1 key) value)) table) (hash 2 "one"))

  (test-equal?
   "Fold works."
   (fold (fn ([state : Integer] [_ : String] [value : Integer]) (+ value state)) 0 add-table)
   6)

  ;(test-equal? "Filter works."
  ;             (filter (fn (pair) (> (cdr pair) 1)) add-table)
  ;             (hash "two" 2 "three" 3))

  ;(test-true "Exists works." (exists (fn (x) (equal? (car x) "one")) add-table))

  (test-equal? "Find works." (find "one" add-table) 1))
