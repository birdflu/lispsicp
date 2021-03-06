#lang scheme

; Define a procedure reverse that takes a list
; as argument and returns a list of the same elements in reverse
; order:

(define (append list1 list2)
  (if (null? list1)
     list2
     (cons (car list1) (append (cdr list1) list2))
     )
  )

(define (reverse items)
  (if (null? (cdr items))
     items
     (append (reverse (cdr items)) (list (car items)))
     )
  )

(reverse (list 1 4 9 16 25))
