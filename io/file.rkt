#lang racket/base
(require racket/file)

(define (file-read-all-lines file-path)
  (file->lines file-path))

(define (file-read-all-bytes file-path)
  (file->bytes file-path))

(module+ test
  (require rackunit)
  (check-equal? (list "1" "2" "3" "4") (file-read-all-lines "test.txt"))
  (check-equal? #"1\r\n2\r\n3\r\n4\r\n" (file-read-all-bytes "test.txt")))
