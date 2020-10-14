#lang scheme

(require rackunit)
(include "../combination.scm")

(define (product term a next b)
    (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (inc x) (+ x 1))

(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))

(define (combination-count-n-k n k)
  (/ (factorial n) (* (factorial (- n k)) (factorial k))))

(define (combination-count n)
  (accumulate + 1 
              (map (lambda (k) (combination-count-n-k n k)) 
                   (enumerate-interval 1 n)))) 


(define result '(() (1) (2) (3) (4) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4) (1 2 3) (1 2 4) (1 3 4) (2 3 4) (1 2 3 4)))

(check-equal? (main '(1 2 3 4)) result)
(check-equal? (length (main '(1 2 3 4 5 6 7 8 9 10))) (combination-count 10))
