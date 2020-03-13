#lang scheme

;Show that we can represent pairs of nonnegative
;integers using only numbers and arithmetic operations
;if we represent the pair a and b as the integer that is
;the product 2a3b . Here the corresponding definitions of the
;procedures cons, car, and cdr.

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (divizor-count z 2 0))

(define (cdr z)
  (divizor-count z 3 0))

(define (divizor-count x divizor count)
  (if (> (remainder x divizor) 0)
      count
      (divizor-count (/ x divizor) divizor (+ count 1))))

(car (cons 3 2))
(cdr (cons 3 2))

(car (cons 1 20))
(cdr (cons 1 20))
