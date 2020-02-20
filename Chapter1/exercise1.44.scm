#lang scheme

; If f is a function and dx is some small number,
; then the smoothed version of f is the function
; whose value at a point x is the average of f(x-dx), f(x) and f(x+dx).
; A procedure smooth takes as input a procedure that computes f
; and returns a procedure that computes the smoothed f.
; Generate the n-fold smoothed function of any given function
; using smooth and repeated procedure from Exercise 1.43.

(define (repeated-smooth f n)
  ((repeated smooth n) f))

(define (smooth f)
  (define (average x y)
    (/ (+ x y) 2))
  (define dx 0.00001)
  (λ(x) (average (f (- x dx)) (f (+ x dx)))))

(define (repeated f n)
  (if (= 1 n) (λ(x) (f x))
          (λ(x) (f ((repeated f (- n 1)) x)))
    ))

(define (square x)
  (* x x))

(square 2)
((smooth square) 2)
((repeated-smooth square 5) 2)
