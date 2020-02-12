#lang scheme

; An infinite continued fraction is an expression of the
; f(k) = N1/(D1 + N2 / ( D2 + ... Nk / Dk)) 
; Leonhard Euler published a memoir De Fractionibus Continuis, which
; included a continued fraction expansion for e - 2, where
; e is the base of the natural logarithms. In this fraction, the
; Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ...
; Procedure recursive approximates e, based on Euler’s expansion.

(define (recursive n d k stop)
  (display (list "   k =" k  "n = " (n k) "d = " (d k)))
  (newline)
  (if (= k stop)
      (/ (n stop) (d stop))
      (/ (n k)
         (+ (recursive n d (+ k 1) stop) (d k))
         )))

(define (Dn n)
  (if (= n 1) 1
      (if (= n 2) 2
          (if (= n 3) 1
              (if (= n 4) 1
                  (if (and (> n 4)(> (Dn (- n 3)) 1))
                      (+ (Dn (- n 1)) (Dn (- n 2)) (Dn (- n 3)))
                      1
                      )))))
  )

(define (loop k stop)
  (display (list "f = " (+ 2 (recursive (λ(i) 1.0) Dn 1.0 k)) "k = " k))
  (newline)
  (newline)
  (if (= k stop)
      (newline)
      (loop (+ k 1) stop)
      )
  
  )

(loop 1 7)

(exp 1.0)
