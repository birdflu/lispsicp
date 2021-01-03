#lang scheme

(require rackunit)
(include "../exercise2.93.scm")
(define p1 (make-polynomial 'x '((2 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational p2 p1))

(check-equal?
  (add rf rf)
  '{rational {polynomial x {5 2} {3 2} {2 2} {0 2}} polynomial x {4 1} {2 2} {0 1}}
  )
