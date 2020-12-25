#lang scheme

(require rackunit)
(include "../exercise2.87.scm")

; polynomial
(install-polynomial-package)

(check-equal?
  (=zero? (make-polynomial 'x '((100 0))))
  '#t)

(check-equal?
  (=zero? (make-polynomial 'x '((1 1) (20 2) (0 1))))
  '#f)

(check-equal?
  (=zero? (make-polynomial 'x '((100 1))))
  '#f)

(check-equal?
  (=zero? (make-polynomial 'x '((1 0) (20 2) (0 1))))
  '#f)

