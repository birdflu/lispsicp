#lang scheme

(require rackunit)
(include "../exercise2.85.scm")

; ordinal
(install-scheme-number-package)

; rational
(install-rational-package)

; complex
(install-complex-package)

; type coercion
(install-type-coercion-package)

(check-equal?  
  (add 5 (make-complex-from-real-imag 2 0) (make-complex-from-real-imag -2 0) -5)
  0)

(check-equal?  
  (mul 5 (make-complex-from-real-imag 1 0) -5 (make-complex-from-real-imag 0.5 0))
  (mcons 'complex (mcons 'polar (mcons 12.5 3.141592653589793))))

(check-equal? 
  (add 5 (make-rational 4 2))
  7)

(check-equal?  
  (add 5 (make-rational 4 3))
  (mcons 'rational (mcons 19 3)))

(check-equal?  
  (equ? (add 5 (make-rational 4 2)) 7)
  #t)

