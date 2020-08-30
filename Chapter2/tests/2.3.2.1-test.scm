#lang scheme

(require rackunit)
(include "../exercise2.3.2.1.scm")


(check-equal? (deriv '(+ x 3) 'x) 1)
(check-equal? (deriv '(* x y) 'x) 'y)
(check-equal? 
  (deriv '(* (* x y) (+ x 3)) 'x)
  '(+ (* x y) (* y (+ x 3))))

