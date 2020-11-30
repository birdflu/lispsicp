#lang scheme
(require rackunit)
(include "../exercise2.73b.scm")

 (check-equal? (deriv '(+ x 3) 'x) 1)

 (check-equal? (deriv 'x 'x) 1)

 (check-equal? (deriv 3 'x) 0)

 (check-equal? 
   (deriv '(* x y) 'x) 
   'y)
 (check-equal? 
   (deriv '(* (* x y) (+ x 3)) 'x) 
   '(+ (* x y) (* y (+ x 3))))
