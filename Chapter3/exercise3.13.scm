#lang scheme
(include "exercise3.12.scm")

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define zz (make-cycle (list 'a 'b 'c)))

zz

(last-pair zz)
