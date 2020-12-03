#lang scheme

(require rackunit)
(include "../exercise2.81-c.scm")

; ordinal
(install-scheme-number-package)

; complex
(install-complex-package)

; type coercion
(install-type-coercion-package)

(check-equal? (add 3 (make-complex-from-real-imag 2 0))
              (make-complex-from-real-imag 5 0))

