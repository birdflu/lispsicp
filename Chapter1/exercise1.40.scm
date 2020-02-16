#lang scheme
; Newton’s method. If x -> g(x) is a differentiable function,
; then a solution of the equation g(x) = 0 is a fixed point of the function x -> f(x)
; where f(x) = x - g(x) / g'(x) and g'(x) is the derivative of g evaluated at x.
; The derivative g'(x) of g is the function whose value at any number x is
;
; g'(x) =  lim  (g(x+dx)-g(x))/dx
;         dx->0
; 
; It tests with a function and finds a roots of (cubic a b c)(x) = x^3+ax^2+bx+c
; If x root then x is fixed point of f(x) = x - g(x)/g'(x), where
; g(x) = x^3+ax^2+bx+c;

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

; Then g(x)=0
(define (cubic-root a b c)
  (newtons-method
   (cubic a b c) 1.0))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(display "g(x): x^3+x^2+x+1")
(newline)
(display (list "root:" (cubic-root 1 1 1)))
(newline)
(display (list "test:" (truncate ((cubic 1 1 1) (cubic-root 1 1 1)))))
 (newline)
(newline)

(display "g(x): x^3+2x^2+3x+10")
(newline)
(display (list "root:" (cubic-root 2 3 10)))
(newline)
(display (list "test:" (truncate ((cubic 2 3 10) (cubic-root 2 3 10)))))
