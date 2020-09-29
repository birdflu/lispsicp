#lang scheme
(require rackunit)
(include "../exercise2.61.scm")

(check-equal? (adjoin-set 3 '(1 3 4)) '(1 3 4))

(check-equal? (adjoin-set 2 '(1 3 4)) '(1 2 3 4))

(check-equal? (adjoin-set 5 '(1 3 4)) '(1 3 4 5))


