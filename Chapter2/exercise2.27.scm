#lang scheme

; procedure that takes a list
; as argument and returns as its value the list with its elements
; reversed and with all sublists deep-reversed as well.

(define (reverse items)
  (if (null? (cdr items))
     items
     (append (reverse (cdr items)) (list (car items)))
     )
  )

(define (deep-reverse items)
  (if (null? (cdr items))
     (list (reverse (car items)))
     (if (pair? (car items))
        (append (deep-reverse (cdr items)) (deep-reverse (car items)))
        (list (reverse items))
        )
     ))

(define x (list (list 1 2) (list 3 4) (list 5 6)  (list 7 8 9))) 
x 
(reverse x) ; ((7 8 9) (5 6) (3 4) (1 2))
(deep-reverse x) ; ((9 8 7) (6 5) (4 3) (2 1))
