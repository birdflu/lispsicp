#lang scheme
(require rackunit)
(include "../exercise2.59.scm")

(check-equal? (union-set '(a) '()) '(a))
(check-equal? (union-set '(a) '(a c)) '(a c))
(check-equal? (union-set '(a) '(b c)) '(a b c))
(check-equal? (union-set '(b c) '(a)) '(b c a))
(check-equal? (union-set '(a b) '(b c)) '(a b c))

