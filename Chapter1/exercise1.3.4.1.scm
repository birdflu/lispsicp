#lang scheme
; Newton’s method. If x -> g(x) is a differentiable function,
; then a solution of the equation g(x) = 0 is a fixed point of the function x -> f(x)
; where f(x) = x - g(x) / g'(x) and g'(x) is the derivative of g evaluated at x.
; The derivative g'(x) of g is the function whose value at any number x is
;
; g'(x) =  lim  (g(x+dx)-g(x))/dx
;         dx->0
; For example, the derivative of x -> x^3 at 5 (whose exact value is 75)
; 
; Then sqrt(x)=y => x=y^2 => 0=y^2-x=g(x)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (cube x) (* x x x))

; With the aid of deriv, we can express Newton’s method as a fixed-point process:
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixedPoint (newton-transform g) guess))

(define (fixedPoint f x)
  (if (close? x (f x)) x
      (fixedPoint f (f x)) 
      )
  )

(define (close? x y) (< (abs (- x y)) tolerance))

(define (square y) (* y y))

(define tolerance 0.00001)

; Then sqrt(x)
(define (sqrt x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

((deriv cube) 5)

(sqrt 25)
