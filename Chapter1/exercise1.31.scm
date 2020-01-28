#lang scheme

; Procedure returns the product of the values of a
; function at points over a given range. Then it define
; factorial in terms of product. Also use product
; to compute approximations π to using the formula
; π/4 = (2*4*4*6*6*8*...)/(3*3*5*5*7*7*...) 


(define (product term a next b)
    (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)) )))
  (iter (next a) (* (term a))  ))

(define (inc x) (+ x 1))

(define (doubleinc x) (+ x 2))

(define (identity x) x)
(define (square x) (* x x))

(define (factorial n)
  (product identity 1 inc n))

(define (π accuracy)
  (* 8 (/ (/ (product square 4 doubleinc accuracy) accuracy) (product square 3 doubleinc (- accuracy 1) ))))

(factorial 4)
(π 240)

(* 1.0 (π 240))
