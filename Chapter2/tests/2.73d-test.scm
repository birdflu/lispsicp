#lang scheme
(require rackunit)
(include "../exercise2.73d.scm")

(install-deriv-package)

(check-equal? (deriv '(+ x 3) 'x) 1)
(check-equal? (deriv 'x 'x) 1)
(check-equal? (deriv 3 'x) 0)

(check-equal? (deriv '(* x y) 'x) 'y)
(check-equal?  (deriv '(* (* x y) (+ x 3)) 'x) 
               '(+ (* x y) (* y (+ x 3))))

(check-equal? (deriv '(** 5 6) 'x) 0)
(check-equal? (deriv '(** x 6) 'x) '(* 6 (** x 5)))
(check-equal? (deriv '(** x y) 'x) '(* y (** x (- y 1))))
(check-equal? (deriv '(** n u) 'x) 0)
