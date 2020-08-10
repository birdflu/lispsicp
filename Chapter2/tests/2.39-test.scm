#lang scheme

(require rackunit)
(include "../2.39.scm")

(define list1 (list 1 2 3))

(check-equal? (reverse list1) (list 3 2 1))
(check-equal? (reverse2 list1) (list 3 2 1))
