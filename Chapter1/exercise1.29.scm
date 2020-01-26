#lang scheme

; This procedure is a method of numerical integration function
; f between a and b is approximated by Simpson’s Rule
; procedure that takes as arguments f, a, b, and n and returns
; the value of the integral, computed using Simpson’s Rule.
; Next, the procedure is used to integrate
; cube between 0 and 1 (with n = 100 and n = 1000),

(define (simpson f a b n)
  (define (coeff k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((even? k) 2)
          (else 4)))
  (define (next n)
    (+ n h))
  (define h (/ (- b a) n))
  (define (term k)
    (* (coeff k) (f (+ a (* k h)))))
  (define (iter i)
    (+ 1 i))
  (* (/ h 3) (sum term 0 iter n)))

(define (sum term a next b)
    (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (cube x)
  (* x x x))

(simpson cube 0 1 10)
