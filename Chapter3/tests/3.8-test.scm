#lang scheme

(require rackunit)
(include "../exercise3.8.scm")
(check-equal?
  (+ (f 1) (f 0))
  1)

(check-equal?
  (+ (f 0) (f 1))
  0)


