#lang scheme

; The fixedPoint is modified so that it prints the
; sequence of approximations it generates, using the newline
; and display primitives

; Then find a solution to x^x = 1000 by finding a fixed point of
; x -> log(1000)/log(x)

(define (fixedPoint f x)
  (display (list "x =" x))
  (newline)
  (if (close? x (f x)) x
      (fixedPoint f (f x)) 
      )
  )

(define tolerance 0.00001)

(define (close? x y) (< (abs (- x y)) tolerance))

(let((result (fixedPoint (λ(x) (/ (log 1000) (log x))) 2)))
  (list "result=" result "; result^result=" (expt result result))
  )
