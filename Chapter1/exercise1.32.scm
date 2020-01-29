#lang scheme

; More general notion called accumulate
; that combines a collection of terms, using some general
; accumulation function: (accumulate combiner null-value term a next b)
; accumulate takes as arguments the same term and
; range specifications as sum and product, together with
; a combiner procedure (of two arguments) that specifies
; how the current term is to be combined with the
; accumulation of the preceding terms and a null-value
; that specifies what base value to use when the terms
; run out.

(define (accumulate combiner null-value term a next b)
    (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (product term a next b)
    (if (> a b)
      1
      (* (term a) (product term (next a) next b))))


(define (identity x) x)
(define (inc x) (+ x 1))

(accumulate + 0 identity 1 inc 5)
(accumulate * 1 identity 1 inc 5)
