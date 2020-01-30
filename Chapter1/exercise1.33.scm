#lang scheme

; The introducing the notion of a filter on the terms to be combined.
; That is, combine only those terms derived from values in the range that satisfy
; a specified condition. The resulting filtered-accumulate
; abstraction takes the same arguments as accumulate, together
; with an additional predicate of one argument that specifies the filter.

(define (accumulate combiner null-value term a next b)
    (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (product term a next b)
    (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m)) m))))

(define (square a)
  (* a a))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (prime? n)
  (fast-prime? n 20))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (filtered-accumulate filter combiner null-value term a next b)
    (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b))
          (filtered-accumulate filter combiner null-value term (next a) next b))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (prime-n? a n)
  (= (gcd a n) 1))

(define (identity x) x)
(define (inc x) (+ x 1))

(define (filtered-product n)
  (define (prime-filter? a)
    (prime-n? a n))
  (filtered-accumulate prime-filter? * 1 identity 1 inc n))

(filtered-accumulate prime? + 0 square 5 inc 12)
(filtered-product 8)

