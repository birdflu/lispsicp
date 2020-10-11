#lang scheme

(require rackunit)
(include "../permutation.scm")

(define list1 (list 1 2 3))

(define (product term a next b)
    (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (inc x) (+ x 1))

(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))

(define (permutation-count-n-k n k)
  (/ (factorial n) (factorial (- n k))))

(define (permutation-count n)
  (accumulate + 1 
              (map (lambda (k) (permutation-count-n-k n k)) 
                   (enumerate-interval 1 n)))) 


(define result '(() (1) (2) (3) (4) 
               (1 2) (1 3) (1 4) (2 1) (2 3) (2 4) 
               (3 1) (3 2) (3 4) (4 1) (4 2) (4 3) 
               (1 2 3) (1 2 4) (1 3 2) (1 3 4) (1 4 2) (1 4 3) 
               (2 1 3) (2 1 4) (2 3 1) (2 3 4) (2 4 1) (2 4 3) 
               (3 1 2) (3 1 4) (3 2 1) (3 2 4) (3 4 1) (3 4 2) 
               (4 1 2) (4 1 3) (4 2 1) (4 2 3) (4 3 1) (4 3 2) 
               (1 2 3 4) (1 2 4 3) (1 3 2 4) (1 3 4 2) (1 4 2 3) 
               (1 4 3 2) (2 1 3 4) (2 1 4 3) (2 3 1 4) (2 3 4 1) 
               (2 4 1 3) (2 4 3 1) (3 1 2 4) (3 1 4 2) (3 2 1 4) 
               (3 2 4 1) (3 4 1 2) (3 4 2 1) (4 1 2 3) (4 1 3 2) 
               (4 2 1 3) (4 2 3 1) (4 3 1 2) (4 3 2 1)))

(define result-text '(() ("a") ("b") ("a" "b") ("b" "a")))

(check-equal? (main '(1 2 3 4)) result)
(check-equal? (main '("a" "b")) result-text)
(check-equal? (length (main '(1 2))) (permutation-count 2))
(check-equal? (length (main '(1 2 3))) (permutation-count 3))
(check-equal? (length (main '(1 2 3 4))) (permutation-count 4))
;(perm  '() '(1 2 3))
;(perm  '(2) '(1 3) )
;(perm '(2 1) '(2 3))
;(perm '(2 1 3) '())
;(permutate  '(( 2 ) ( 1 3 )))
;(permutate  '(( 2 1 ) ( 3 )))
;(permutate  '((1 2 3) ( )))
;(permutation  '(((2) (1 3))))
;(length (main '(1 2 3 4 5 6 7 8 9 10)))
