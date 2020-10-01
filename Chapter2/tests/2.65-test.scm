#lang scheme
(require rackunit)
(include "../exercise2.65.scm")

(define tree1 (list->tree '(1 3 5 7 9 11)))
(define tree2 (list->tree '(3 5 7 9 11 13)))

(check-equal? 
 (union-tree tree1 tree2)
 '(7 (3 (1 () ()) (5 () ())) (11 (9 () ()) (13 () ()))))

(check-equal? 
 (tree->list (union-tree tree1 tree2))
 '(1 3 5 7 9 11 13))

(check-equal? 
 (intersection-tree tree1 tree2)
 '(7 (3 () (5 () ())) (9 () (11 () ()))))

(check-equal? 
 (tree->list (intersection-tree tree1 tree2))
 '(3 5 7 9 11))
