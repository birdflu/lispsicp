#lang scheme

; An infinite continued fraction is an expression of the
; f(k) = N1/(D1 + N2 / ( D2 + ... Nk / Dk)) 
; the infinite continued fraction expansion with the Ni and the Di all
; equal to 1 produces 1=φ, where φ is the golden ratio.
; The procedure (calcContinuedFraction n d k)
; computes the value of the k-term finite continued fraction.
; Check our procedure by approximating 1=φ

; for recursive f(k) => f = Nk/(Dk + N(k-1) / ( D(k-1) + ... N1 / D1))

(define (iterate n d k)
  (define (calculate n d i)
    (if (< i k)
        (/ (n i)
           (+ (d i)
              (calculate n d (+ i 1))
              ))
        (/ (n i) (d i))
        )
    )

  (calculate n d 1)
  )

(define (recursive n d k)
  (if (= k 1)
      (/ (n 1) (d 1))
      (/ (n k)
         (+ (recursive n d (- k 1)) (d k))
      )))

(let ((φ 1.618))
  (/ 1 φ))

(iterate (λ(i) 1.0) (λ(i) 1.0) 15.0)

(recursive (λ(i) 1.0) (λ(i) 1.0) 15.0)
