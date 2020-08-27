#lang scheme
(require rackunit)
(include "../exercise2.54.scm")

(check-equal? (equal? '(this is a list) '(this is a list)) #t)
(check-equal? (equal? '(this is a list) '(this (is a) list)) #f)

