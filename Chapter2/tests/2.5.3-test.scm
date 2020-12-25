#lang scheme

(require rackunit)
(include "../exercise2.5.3.scm")

; polynomial
(install-polynomial-package)

(check-equal?
  (make-polynomial 'x '((100 1) (2 2) (0 1)))
  (mcons
    'polynomial
    (mcons
      'x
      (mcons
        (mcons 100 (mcons 1 '()))
        (mcons (mcons 2 (mcons 2 '())) (mcons (mcons 0 (mcons 1 '())) '()))))))

(check-equal?
  (make-polynomial 'x '((100 0)))
  (mcons 'polynomial (mcons 'x (mcons (mcons 100 (mcons 0 '())) '()))))

