#lang racket/base
(require racket/file)

(define (read-all-lines file-path)
  (file->lines file-path))

(define (read-all-bytes file-path)
  (file->bytes file-path))

(module+ test
  (require rackunit)
  (check-equal? (list "1" "2" "3" "4") (read-all-lines "test.txt"))
  (check-equal? #"1\r\n2\r\n3\r\n4\r\n" (read-all-bytes "test.txt")))
