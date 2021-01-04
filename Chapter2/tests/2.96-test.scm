#lang scheme

(require rackunit)
(include "../exercise2.96.scm")

(define p1 (make-polynomial 'x '(1 -2 1)))
(define p2 (make-polynomial 'x '(11 0 1)))
(define p3 (make-polynomial 'x '(13 5)))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(check-equal?
  (greatest-common-divisor q1 q2)
  p1 
  )

