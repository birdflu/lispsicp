#lang scheme

; A number x is called a fixed point of a function f if x satisfies the equation
; f (x) = x. For some functions f we can locate a fixed point by
; beginning with an initial guess and applying f repeatedly,
; f(x), f(f(x)), f(f(f(x))), ...
; until the value does not change very much. Using this idea, 
; a procedure fixedPoint takes as inputs a function and an
; initial guess and produces an approximation to a fixed point of the function.

(define (fixedPoint f x)
  (if (close? x (f x)) x
      (fixedPoint f (f x)) 
      )
  )

(define tolerance 0.00001)

(define (close? x y) (< (abs (- x y)) tolerance))

(fixedPoint (λ(x) (+ (sin x) (cos x))) 1.0)
