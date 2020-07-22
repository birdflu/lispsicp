#lang scheme
; the count-leaves procedure,
; which returns the total number of leaves of a tree

(define nil '())

(define (count-leaves x)
  (if (null? x)
     0
     (if (pair? (car x))
        (+ (count-leaves (car x)) (count-leaves (cdr x)))
        (+ 1 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))
(length x)
(count-leaves x)

(list x x)
(length (list x x))
(count-leaves (list x x))
