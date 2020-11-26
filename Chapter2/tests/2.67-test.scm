#lang scheme
(require rackunit)
(include "../exercise2.67.scm")

(check-equal? 
 (decode sample-message sample-tree)
 '(A D A B B C A))

