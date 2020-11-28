#lang scheme

(require rackunit)
(include "../exercise2.4.2.scm")

(define n (make-from-real-imag 5 2))  
(define m (make-from-real-imag 2 1))
(define np (make-from-mag-ang (magnitude n) (angle n)))
(define mp (make-from-mag-ang (magnitude m) (angle m)))
(define n2 (make-from-real-imag (real-part n) (imag-part n)))
(define m2 (make-from-real-imag (real-part m) (imag-part m)))

(check-equal? n n2)
(check-equal? m m2)
