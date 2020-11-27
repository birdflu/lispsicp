#lang scheme
(require rackunit)
(include "../exercise2.69.scm")

(define sample-tree-2-69
'((leaf A 4)
  ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4)
  (A B D C)
  8))

(define sample-2-69 '((A 4) (B 2) (C 1) (D 1)))

(check-equal? (generate-huffman-tree sample-2-69) sample-tree-2-69) 
  

