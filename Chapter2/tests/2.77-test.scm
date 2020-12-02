#lang scheme

(require rackunit)
(include "../exercise2.77.scm")

(install-complex-package)

(define n (make-complex-from-real-imag 5 2))  
(define m (make-complex-from-real-imag 2 1))
(define np (make-complex-from-mag-ang (magnitude n) (angle n)))
(define mp (make-complex-from-mag-ang (magnitude m) (angle m)))
(define n2 (make-complex-from-real-imag (real-part n) (imag-part n)))
(define m2 (make-complex-from-real-imag (real-part m) (imag-part m)))

(check-equal? n n2)
(check-equal? m m2)
