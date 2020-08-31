#lang scheme
(require rackunit)
(include "../exercise2.58util.scm")

(check-equal? 
  (infix-to-prefix '(x + (3 * (x + (y + 2)))))
  '(+ x (* 3 (+ x (+ y 2)))))
(check-equal? 
  (infix-to-prefix '((x * y) * (x + 3)) )
  '(* (* x y) (+ x 3)))
