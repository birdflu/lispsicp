#lang scheme
(require rackunit)
(include "../exercise2.58a.scm")

;(check-equal? (deriv '((x * y) * (x + 3)) 'x)
;              (deriv '(* (* x y) (+ x 3)) 'x))
;(check-equal? (deriv '(+ x y (+ x 3)) 'x)
;              (deriv '(+ (+ x y) (+ x 3)) 'x))
(check-equal? (deriv '(x + (3 * (x + (y + 2)))) 'x)
              (prefix-deriv '(+ x (* 3 (+ x (+ y 2)))) 'x))
(check-equal? (deriv '((x * y) * (x + 3)) 'x)
              (prefix-deriv '(* (* x y) (+ x 3)) 'x))
