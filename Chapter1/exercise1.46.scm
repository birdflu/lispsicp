#lang scheme

; The procedure iterative-improve takes two procedures
; as arguments: a method for telling whether a guess is good
; enough and a method for improving a guess. iterativeimprove
; should return as its value a procedure that takes a
; guess as argument and keeps improving the guess until it is
; good enough. Then we rewrite the sqrt procedure 
; and the fixed-point procedure in terms of iterative-improve.

(define (sqrt x)
  (define (sqrt-iter guess x)
    (define (improve guess)
      (average guess (/ x guess)))
    (define (good-enough? guess)
      (< (abs (- (* guess guess) x)) tolerance))
    ((iterative-improve good-enough? improve) guess))
  (sqrt-iter 1 x))

(define (fixed-point f first-guess)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve close-enough? f) first-guess))

(define (iterative-improve good-enough? improve)
  (λ(x) (define (iter x)
          (let ((new-x (improve x)))
            (if (good-enough? new-x)
                new-x
                (iter new-x))
            ))
    (iter x)
    ))

(define tolerance 0.00001)

(define (average x y)
  (/ (+ x y) 2.0))

(sqrt 9)
(fixed-point cos 1)
