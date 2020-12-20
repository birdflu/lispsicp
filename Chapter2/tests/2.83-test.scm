#lang scheme

(require rackunit)
(include "../exercise2.83.scm")

; ordinal
(install-scheme-number-package)

; rational
(install-rational-package)

; complex
(install-complex-package)

; type coercion
(install-type-coercion-package)

(check-equal?  
  (raise-one-level 4)
  (mcons 'rational (mcons 4 1)))

(check-equal?  
  (raise-one-level (make-rational 4 5))
  (mcons 'complex (mcons 'rectangular (mcons 4/5 0))))

(check-equal?  
  (raise-one-level (make-complex-from-real-imag 4 5))
  (mcons 'complex (mcons 'rectangular (mcons 4 5))))



