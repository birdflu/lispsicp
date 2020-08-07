#lang scheme

(require rackunit)
(include "../2.37.scm")

(check-equal? (dot-product v w) 54 "dot-product")
(check-equal? (matrix-*-vector m v) (list 10 21 30) "matrix-*-vector")
