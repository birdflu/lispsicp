#lang scheme
(require rackunit)
(include "../exercise2.60.scm")

(define a '(2 3 2 1 3 2 2))

(check-equal? (element-of-set? 3 a)
#t)

(check-equal? (adjoin-set 5 a)
'(5 2 3 2 1 3 2 2))

(check-equal? (adjoin-set 3 a)
'(3 2 3 2 1 3 2 2))

(check-equal? (union-set '(5) a)
'(5 2 3 2 1 3 2 2))

(check-equal? (union-set a a)
'(2 3 2 1 3 2 2 2 3 2 1 3 2 2))

(check-equal? (intersection-set '(2 3 2) a) 
'(2 3 2))

(check-equal? (intersection-set '( 7 2 3 2 9 8) a) 
'(2 3 2))
