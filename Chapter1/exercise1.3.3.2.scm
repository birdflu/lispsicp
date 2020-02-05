#lang scheme

; The halfInterval method is a simple but powerful technique for finding
; roots of an equation f (x) = 0, where f is a continuous function. The
; idea is that, if we are given points a and b such that f (a) < 0 < f (b),
; then f must have at least one zero between a and b. To locate a zero,
; let x be the average of a and b, and compute f (x). If f (x) > 0, then
; f must have a zero between a and x. If f (x) < 0, then f must have a
; zero between x and b. Continuing in this way, we can identify smaller
; and smaller intervals on which f must have a zero. When we reach a
; point where the interval is small enough, the process stops.

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
