#lang scheme

(require rackunit)
(include "../exercise2.84.scm")

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
  (mcons 'complex (mcons 'rectangular (mcons 0 0))))

(check-equal?  
  (mul 5 (make-complex-from-real-imag 1 0) -5 (make-complex-from-real-imag 0.5 0))
  (mcons 'complex (mcons 'polar (mcons 12.5 3.141592653589793))))

(check-equal? 
  (div 5 (make-complex-from-real-imag 1 0) -5 (make-complex-from-real-imag 0.5 0))
  (mcons 'complex (mcons 'polar (mcons 2.0 -3.141592653589793))))

(check-equal?  
  (sub 5 (make-complex-from-real-imag 1 0) -5 (make-complex-from-real-imag 0.5 0))
  (mcons 'complex (mcons 'rectangular (mcons 8.5 0))))
 
(check-equal?  
  (equ?
   (add 5 (make-complex-from-real-imag 2 0) (make-complex-from-real-imag -2 0) -5)
   (add -5 (make-complex-from-real-imag 2 0) (make-complex-from-real-imag -2 0) 5))
  #t)

(check-equal? 
  (=zero? 0)
  #t)

(check-equal? 
  (=zero? (make-complex-from-real-imag 0 0))
  #t)

(check-equal? 
  (=zero? 1)
  #f)


