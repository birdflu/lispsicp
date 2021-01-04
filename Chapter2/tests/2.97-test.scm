#lang scheme

(require rackunit)
(include "../exercise2.97.scm")

(define p1 (make-polynomial 'x '((1 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1) (0 -1))))
(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

(check-equal?
  (add rf1 rf2)
  '{rational {polynomial x {4 1} {3 1} {2 1} {1 -2} {0 -1}} polynomial x {5 1} {3 -1} {2 -1} {0 1}}   )

