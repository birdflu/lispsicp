#lang scheme

; Here is an alternative procedural representation
; of pairs. 

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(car (cons 3 4))
(cdr (cons 3 4))

