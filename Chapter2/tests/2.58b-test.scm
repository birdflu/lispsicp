#lang scheme
(require rackunit)
(include "../exercise2.58b.scm")

(check-equal? (deriv '(x + 3 * (x + y + 2)) 'x)
              (prefix-deriv  '(+ x (* 3 (+ x (+ y 2)))) 'x))
