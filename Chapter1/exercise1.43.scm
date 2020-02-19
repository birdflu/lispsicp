#lang scheme

; Define a procedure repeated -
; the function whose value at x is f(f(...(f(x))...)) n times.

(define (repeated f n)
  (if (= 1 n) (λ(x) (f x))
          (λ(x) (f ((repeated f (- n 1)) x)))
    ))

(define (square x)
  (* x x))

(define (double x)
  (+ x x))


((repeated square 2) 5)

((repeated double 2) 5)
((repeated double 3) 5)
