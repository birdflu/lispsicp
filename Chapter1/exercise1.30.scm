#lang scheme

; The procedure is rewriten so that the sum
; is performed iteratively instead generates linear recursion.
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)) )))
  (iter (next a) (+ (term a))))

(define (sum term a next b)
    (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (sum-cub a b)
  (sum cube a inc b))

(define (cube x)
  (* x x x))

(define (inc x) (+ x 1))

(sum-iter cube 1 inc 10)
