#lang racket

(require ffi/unsafe
         ffi/unsafe/define)

(define-ffi-definer define-other (ffi-lib "other_ffi.dll"))
(define-ffi-definer define-more (ffi-lib "more_ffi.dll"))
(define-ffi-definer define-algo (ffi-lib "low_level_algo.dll"))

(define-other max (_fun _gcpointer _size -> _int))
(define-more min (_fun _gcpointer _size -> _int))
(define-algo reverse_array_better (_fun _gcpointer _size -> _gcpointer))

(define items (malloc 'raw 3))

(memset items 0 4 _int)

(ptr-set! items _int 0 6)
(ptr-set! items _int 1 -55)
(ptr-set! items _int 2 -3)

(max items 3)
(min items 3)

(define result (reverse_array_better items 3))

(define items-list (list (ptr-ref result _int 0) (ptr-ref result _int 1) (ptr-ref result _int 2)))

(free items)

items-list

(free result)
