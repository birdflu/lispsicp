#lang scheme

;The procedure for-each is similar to map. It
; takes as arguments a procedure and a list of elements. However,
; rather than forming a list of the results, for-each just
; applies the procedure to each of the elements in turn, from
; left to right.
; Give an implementation of for-each.

(define nil '())

(define (for-each proc items)
  (cond ((null? items) items)
       (else (for-each proc (cdr items)) (proc (car items)))))

(for-each (lambda (x)
            (newline)
            (display x))
         (list 57 321 88))

