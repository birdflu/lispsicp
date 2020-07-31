#lang scheme

(define (map proc items)
  (if (null? items)
     items
     (cons (proc (car items)) (map proc (cdr items)))))

(define (proc x)
  (* x x))

(define (walk tree)
  (cond ((null? tree) tree)
       ((pair? tree) (cons (car tree) (walk (cdr tree))))
       (else (cons (car tree) (cdr tree))))
  )

(define (scale-tree  tree factor)
  (cond ((null? tree) tree)
       ((pair? tree) (cons (scale-tree (car tree) factor) (scale-tree (cdr tree) factor)))
       (else (* factor tree))))

(define (scale-tree-map tree factor)
  (map (λ (sub-tree)
         (if (pair? sub-tree)
            (scale-tree-map sub-tree factor)
      (* sub-tree factor)))
      tree))

(define test-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
  
(map proc (list 1 2 3))
test-tree
(walk test-tree)
(scale-tree test-tree 10)
(scale-tree-map test-tree 10)
