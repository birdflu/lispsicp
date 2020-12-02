#lang scheme

(require rackunit)
(include "../exercise2.4.2.scm")

(define n (make-from-real-imag 5 2))  
(define np (make-from-mag-ang (magnitude n) (angle n)))
(define n2 (make-from-real-imag (real-part n) (imag-part n)))

(check-equal? (real-part n) (real-part n2))
(check-equal? (imag-part n) (imag-part n2))
(check-equal? (magnitude n) (magnitude n2))
(check-equal? (angle n) (angle n2))
