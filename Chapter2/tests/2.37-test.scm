#lang scheme

(require rackunit)
(include "../2.37.scm")

(define v (list 1 1 1 1))
(define w (list 2 2 2 2))
(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define n (list (list 1 4 6) (list 2 5 7) (list 3 6 8) (list 4 6 9)))

(check-equal? (dot-product v w) 8 "dot-product")
(check-equal? (matrix-*-vector m v) (list 10 21 30) "matrix-*-vector")
(check-equal? (transpose m) n "transpose")
(check-equal? (matrix-*-matrix m n) (list (list 30 56 80) (list 56 113 161) (list 80 161 230)) "matrix-*-matrix")
