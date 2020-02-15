#lang scheme
; Since Newton’s method was itself expressed
; as a fixed-point process, we actually saw two ways to compute square
; roots as fixed points. Each method begins with a function and finds a
; fixed point of some transformation of the function. We can express this
; general idea itself as a procedure
; Procedure takes as its arguments a procedure g that computes some function,
; a procedure that transforms g, and an initial guess.
; The returned result is a fixed point of the transformed function.

(define (fixed-point-of-transform g transform guess)
  (fixedPoint (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x)) newton-transform 1.0))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (fixedPoint f x)
  (if (close? x (f x)) x
      (fixedPoint f (f x)) 
      )
  )

(define (close? x y) (< (abs (- x y)) tolerance))

(define (square y) (* y y))

(define dx 0.00001)
(define tolerance 0.00001)

(sqrt 25)
