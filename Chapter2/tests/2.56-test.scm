#lang scheme
(require rackunit)
(include "../exercise2.56.scm")

(check-equal? (deriv '(** 5 6) 'x) 0)
(check-equal? (deriv '(** x 6) 'x) '(* 6 (** x 5)))
(check-equal? (deriv '(** x y) 'x) '(* y (** x (- y 1))))
(check-equal? (deriv '(** n u) 'x) 0)
