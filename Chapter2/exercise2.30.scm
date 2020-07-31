#lang scheme
; Define a procedure square-tree analogous
; to the square-list procedure of Exercise 2.21.
; Define square-tree both directly (i.e., without using any
; higher-order procedures) and also by using map and recursion.

(define (map proc items)
  (if (null? items)
     items
     (cons (proc (car items)) (map proc (cdr items)))))

(define (walk tree)
  (cond ((null? tree) tree)
       ((pair? tree) (cons (car tree) (walk (cdr tree))))
       (else (cons (car tree) (cdr tree))))
  )

(define (square-tree tree)
  (cond ((null? tree) tree)
       ((pair? tree) (cons (square-tree (car tree)) (square-tree (cdr tree))))
       (else (* tree tree))))

(define (square-tree-map tree)
  (map (λ (sub-tree)
         (if (pair? sub-tree)
            (square-tree-map sub-tree)
            (* sub-tree sub-tree)))
      tree))

(define test-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
  
test-tree
(walk test-tree)
(square-tree test-tree) ; (1 (4 (9 16) 25) (36 49))
(square-tree-map test-tree) ; (1 (4 (9 16) 25) (36 49))
