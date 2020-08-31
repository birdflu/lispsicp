#lang scheme
(require rackunit)
(include "../exercise2.57.scm")

(check-equal? (deriv '(* x y (+ x 3)) 'x)
              (deriv '(* (* x y) (+ x 3)) 'x))
(check-equal? (deriv '(+ x y (+ x 3)) 'x)
              (deriv '(+ (+ x y) (+ x 3)) 'x))

