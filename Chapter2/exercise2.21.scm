#lang scheme

;The procedure square-list takes a list of
;numbers as argument and returns a list of the squares of
;those numbers.
(define nil '())

(define (square-list1 items)
  (if (null? items)
     nil
     (cons (* (car items) (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
  (map (λ(x) (* x x)) items))

(square-list1 (list 1 2 3 4))
(square-list2 (list 1 2 3 4))
