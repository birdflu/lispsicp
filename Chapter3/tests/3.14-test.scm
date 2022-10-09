#lang scheme

(require rackunit)
(include "../exercise3.14.scm")

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

(check-equal?
  v
  '(a)
  )

(check-equal?
  w
  '(d c b a)
  )

