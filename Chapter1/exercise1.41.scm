#lang scheme

; Define a procedure double that takes a procedure
; of one argument as argument and returns a procedure
; that applies the original procedure twice. For example,
; if inc is a procedure that adds 1 to its argument, then
; (double inc) should be a procedure that adds 2.

; What value is returned by (((double (double double)) inc) 5)?

(define (double f)
  (λ(x) (f (f x))))

(define (inc arg)
  (+ arg 1))

(inc 5)
(double inc)
((double inc) 5)
(((double (double double)) inc) 5)
