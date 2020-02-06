#lang scheme

; A number x is called a fixed point of a function f if x satisfies the equation
; f (x) = x. For some functions f we can locate a fixed point by
; beginning with an initial guess and applying f repeatedly,
; f(x), f(f(x)), f(f(f(x))), ...
; until the value does not change very much. Using this idea, 
; a procedure fixedPoint takes as inputs a function and an
; initial guess and produces an approximation to a fixed point of the function.

(define (halfInterval f left right)
  (let ((avg (/ (+  left right) 2))
        )
    (define next (f avg))
    (display (list "leftArg =" left " rightArg =" right "f(avg ="avg ")=" next))
    (newline)
    
    (if (close? left right) avg
        (if (< 0 next)
            (halfInterval f left avg)
            (halfInterval f avg right )
            )
        )
    )
  )
(define tolerance 0.0001)

(define (close? x y) (< (abs (- x y)) tolerance))

(halfInterval sin 6 6.5)
