#lang scheme

(require rackunit)
(include "../exercise2.88.scm")

; polynomial
(install-polynomial-package)

(define a (make-polynomial 'x '((1 1) (20 2) (0 1))))
(define b (make-polynomial 'x '((1 1))))
(define c (make-polynomial 'y '((1 1))))

(check-equal? (sub a a) '{polynomial x})

(check-pred =zero? (sub a a))

(check-equal? (sub a b) '{polynomial x {20 2} {0 1}})

;(check-exn exn:fail? (sub a c))
