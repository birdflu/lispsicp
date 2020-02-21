#lang scheme

; Compute n-th roots as a fixed point search
; based upon repeated average damping of y -> x/y^(n-1).
; Use this to implement a simple procedure for computing n-th
; roots using fixed-point, average-damp, and
; the repeated procedure of Exercise 1.43.

(define (repeated f n)
  (if (= 1 n) (λ(x) (f x))
      (λ(x) (f ((repeated f (- n 1)) x)))
      ))

(define tolerance 0.00001)

(define (fixedPoint f x)
  (if (close? x (f x)) x
      (fixedPoint f (f x)) 
      )
  )

(define (close? x y) (< (abs (- x y)) tolerance))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y)
  (/ (+ x y) 2.0))

(define (root x n)
  (fixedPoint ((repeated average-damp 5) (lambda (y) (/ x (expt y (- n 1)))))
              1.0))

(root 81 4)

(root 256 8)

(root 1024 10)

(root 1048576 20)

(root 4294967296 32)

(root 3486784401 20)
