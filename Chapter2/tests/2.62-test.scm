
#lang scheme
(require rackunit)
(include "../exercise2.62.scm")

(check-equal? (union-set '(1 2) '(3 4)) '(1 2 3 4))
(check-equal? (union-set '(1 2) '(2 3)) '(1 2 3))
(check-equal? (union-set '(1 2) '(1 3)) '(1 2 3))
(check-equal? (union-set '() '(1 3)) '(1 3))




