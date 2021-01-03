#lang scheme

(require rackunit)
(include "../exercise2.94.scm")
(define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-polynomial 'x '((3 1) (1 -1))))
(define p3 (make-polynomial 'x '((3 1) (2 -1) (1 -5) (0 -3))))
(define p4 (make-polynomial 'x '((2 1) (1 1) (0 -12))))
(define p5 (make-polynomial 'x '(1 -1 -5 -3)))
(define p6 (make-polynomial 'x '(1 1 -12)))

(check-equal?
  (greatest-common-divisor p1 p2)
  '{polynomial x {2 -1} {1 1}}
  )

(check-equal?
  (greatest-common-divisor p3 p4)
  '{polynomial x {1 9} {0 -27}}
  )

(check-equal?
  (greatest-common-divisor p3 p4)
  '{polynomial x {1 9} {0 -27}}
  )



