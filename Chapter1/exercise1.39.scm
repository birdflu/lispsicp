#lang scheme

; An infinite continued fraction is an expression of the
; f(k) = N1/(D1 + N2 / ( D2 + ... Nk / Dk)) 
; A continued fraction representation of the tangent function
; J.H. Lambert tg(x) = x / (1 - x^2 / (3 - x^2 / 5 - ... ))
; where x is in radians.
; Procedure recursive) computes an approximation to the tangent
; function based on Lambert’s formula.


(define (recursive n x d k stop)
  (display (list "   k =" k  "n = " (n k x) "d = " (d k)))
  (newline)
  (if (= k stop)
      (/ (n stop x) (d stop))
      (/ (n k x)
         (- (d k) (recursive n x d (+ k 1) stop))
         )))

(define (Dn n)
  (- (* 2 n) 1))

(define (Nn n x)
  (if (= n 1)
      x
      (* x x)))

(define (loop x k stop)
  (display (list "f = " (recursive Nn x Dn 1.0 k) "k = " k))
  (newline)
  (newline)
  (if (= k stop)
      (newline)
      (loop x (+ k 1) stop)
      )
  
  )

(loop 0.5 1 7)

(tan 0.5)
