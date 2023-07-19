#lang racket/base

(require (prefix-in Array. "../collections/array.rkt"))
(require (prefix-in List. "../collections/list.rkt"))
(require (prefix-in Seq. "../collections/seq.rkt"))
(require (prefix-in Map. "../collections/map.rkt"))
(require threading
         "../globals.rkt")

; Map and filter on an array.
(~>> #(1 2 3 4)
     (Array.map (fn (x) (+ x 1)))
     (Array.filter (fn (x) (> x 1)))
     (Array.map (fn (x) (- x 1)))
     (Array.filter (fn (x) (= (modulo x 2) 0))))

; Map and filter on a list.
(~>> '(1 2 3 4)
     (List.map (fn (x) (+ x 1)))
     (List.sort-ascending)
     (List.filter (fn (x) (= (modulo x 2) 0))))

(List.append '(1 2 3) '(2 3 4))
