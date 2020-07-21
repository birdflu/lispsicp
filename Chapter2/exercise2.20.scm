#lang scheme

; The procedures +, *, and list take arbitrary
; numbers of arguments. One way to define such procedures
; is to use define with dotted-tail notation.
; Use this notation to write a procedure same-parity that
; takes one or more integers and returns a list of all the arguments
; that have the same even-odd parity as the first
; argument.

(define (same-party x . l)
  (if (odd? x)
     (cons x (get-odd l))
     (cons x (get-even l))
     ))

(define (get-odd items)
  (if (null? items)
     items
     (if (odd? (car items))
        (cons (car items) (get-odd (cdr items)))
        (get-odd (cdr items))) 
     ))

(define (get-even items)
  (if (null? items)
     items
     (if (even? (car items))
        (cons (car items) (get-even (cdr items)))
        (get-even (cdr items))) 
     ))

(get-odd (list 1 2 3 4 5))
(get-even (list 1 2 3 4 5))
(same-party 1 2 3 4 5)
(same-party 2 3 4 5 6 7)
