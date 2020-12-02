#lang scheme

(require rackunit)
(include "../exercise2.78.scm")

(install-complex-package)

(define n (make-complex-from-real-imag 5 2))  
(define m (make-complex-from-real-imag 2 1))
(define np (make-complex-from-mag-ang (magnitude n) (angle n)))
(define mp (make-complex-from-mag-ang (magnitude m) (angle m)))
(define n2 (make-complex-from-real-imag (real-part n) (imag-part n)))
(define m2 (make-complex-from-real-imag (real-part m) (imag-part m)))

(check-equal? n n2)
(check-equal? m m2)
(check-equal? (add n m) (make-complex-from-real-imag 7 3))
(check-equal? (sub n m) (make-complex-from-real-imag 3 1))

(install-scheme-number-package)

(check-equal? (add 1 3) 4)
(check-equal? (mul 4 3) 12)
