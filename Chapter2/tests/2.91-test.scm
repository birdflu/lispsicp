#lang scheme

(require rackunit)
(include "../exercise2.91.scm")

; polynomial
(install-polynomial-package)
(define p1 (make-polynomial 'x '((5 1) (0 -1))))
(define p2 (make-polynomial 'x '((2 1) (0 -1))))
(define p3 (make-polynomial 'x '(2 -1 -1 3 -30)))
(define p4 (make-polynomial 'x '(1 2)))
(define p5 (make-polynomial 'x '{{0 -30} {1 3} {2 -1} {3 -1} {4 2}}))
(define p6 (make-polynomial 'x '{{0 2} {1 1}}))

(check-equal?
  (div p1 p1)
  '{{polynomial x {0 1}} {polynomial x}}
  )

(check-equal?
  (div p1 p2)
  '{{polynomial x {3 1} {1 1}} {polynomial x {1 1} {0 -1}}}
  )

(check-equal?
  (div p5 p4)
  '{{polynomial x {0 -15} {1 9} {2 -5} {3 2}} {polynomial x}}
  )

(check-equal?
  (div p3 p4)
  '{{polynomial x 2 -5 9 -15} {polynomial x}}
  )

(check-equal?
  (div p3 p6)
  '{{polynomial x 2 -5 9 -15} {polynomial x}}
  )

