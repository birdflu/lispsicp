#lang scheme

; Let f and g be two one-argument functions.
; The composition f and g is defined to be the function x -> f(g(x))
; Define a procedure compose that implements composition.

(define (compose f g)
  (λ(x) (f (g x))))

(define (inc arg)
  (+ arg 1))

(define (square x)
  (* x x))

((compose square inc) 6)
