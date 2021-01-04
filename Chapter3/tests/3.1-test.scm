#lang scheme

(require rackunit)
(include "../exercise3.1.scm")

(define A (make-accumulator 5))

(check-equal?
  (A 10)
  15
 )

(check-equal?
  (A 10)
  25
 )

