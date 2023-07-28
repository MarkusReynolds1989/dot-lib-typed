#lang racket

(define packages
  (list "src/globals.rkt"
        "src/collections/map.rkt"
        "src/collections/list.rkt"
        "src/collections/array.rkt"
        "src/collections/seq.rkt"
        "src/io/file.rkt"))

(for-each (lambda (x)
            (displayln (~a "Building: " x))
            (system (~a "raco make " x)))
          packages)

(for-each (lambda (x)
            (displayln (~a "Testing: "))
            (system (~a "raco test " x)))
          packages)
