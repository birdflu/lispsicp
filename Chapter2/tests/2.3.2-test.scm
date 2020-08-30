#lang scheme

(require rackunit)
(include "../exercise2.3.2.scm")


(check-equal? 
  (deriv '(+ x 3) 'x)
  '(+ 1 0))
(check-equal? 
  (deriv '(* x y) 'x)
  '(+ (* x 0) (* 1 y)))
(check-equal? 
  (deriv '(* (* x y) (+ x 3)) 'x)
  '(+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3))))
;
