#lang scheme
; The simple training for list 

(define nil '())
(define l1 (list 1 2 3 4 ))
(define l2 (cons 1 (cons 2 (cons 3 ( cons 4 nil)))))
(define message (list l1 'is 'equal l2))
message

(car l1) ; 1 expected
(cdr l1) ; (2 3 4) expected
(cons 0 l1) ; (0 1 2 3 4) expected

(define (list-ref items n)
  (if (= n 0)
     (car items)
     (list-ref (cdr items) (- n 1))))

(list-ref l1 2)  ; 3 expected

(define (length items)
  (if (null? items)
     0
     (+ 1 (length (cdr items)))))

(length l1)  ; 4 expected

(define (append list1 list2)
  (if (null? list1)
     list2
     (cons (car list1) (append (cdr list1) list2))
     )
  )

(append (list 0 1 2) (list 3 4))
