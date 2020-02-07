#lang scheme

; Show that the golden ratio φ ~ 1.6180
; is a fixed point of the transformation x -> 1 + 1/x, and
; use this fact to compute φ by means of the fixed-point
; procedure.

; φ is the root of φ^2 = φ + 1;
; So divide both parts by φ:
; φ -> 1 + 1/φ

(define (fixedPoint f x)
  (if (close? x (f x)) x
      (fixedPoint f (f x)) 
      )
  )

(define tolerance 0.00001)

(define (close? x y) (< (abs (- x y)) tolerance))

(fixedPoint (λ(φ) (+ 1 (/ 1 φ))) 2)
