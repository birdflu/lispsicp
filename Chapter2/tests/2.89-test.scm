#lang scheme

(require rackunit)
(include "../exercise2.89.scm")

; polynomial
(install-polynomial-package)

(define a (make-polynomial 'x '(3 0 0 5)))
(define b (make-polynomial 'x '(3 2 1 5)))

(check-equal? a '{polynomial x {0 5} {3 3}})
(check-equal? b '{polynomial x {0 5} {1 1} {2 2} {3 3}})
