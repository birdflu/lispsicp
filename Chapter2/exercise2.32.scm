#lang scheme
; We can represent a set as a list of distinct
; elements, and we can represent the set of all subsets of the
; set as a list of lists.

(define (subsets s)
  (if (null? s)
     (list '())
     (let ((rest (subsets (cdr s))))
       (append rest
              (map
               (λ (x) (cons (car s) x))
               rest)))))

(define test (list 1 2 3))
  
test
(subsets test) ; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
