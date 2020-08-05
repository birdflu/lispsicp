#lang scheme
(include "2.2.3.1.rkt")

; evaluate the
; polynomial using a well-known algorithm called Horner’s rule,

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
             0
             coefficient-sequence))

; compute 1+3x +5x^3 +x^5 at x = 2
; 1 + 3*2 + 5*8 + 32 = 79
; 1 + 2(3 + 2(0 + 2(5 + 2(0 + 2*1)))) = 79 
(horner-eval 2 (list 1 3 0 5 0 1))


