#lang scheme

; Define a procedure repeated -
; the function whose value at x is f(f(...(f(x))...)) n times.

(define (repeated f n)
  (if (= 1 n) (λ(x) (f x))
      (if (= 2 n) (λ(x) (f (f x)))
          (repeated f (- n 1)))))

(define (square x)
  (* x x))

((repeated square 2) 5)
