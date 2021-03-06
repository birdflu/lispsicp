#lang scheme

(include "../2.40.scm")
(include "2.2.3.2-test.scm")
(require rackunit)

(define list1 (list
               (list 2 1)
               (list 3 1)
               (list 3 2)
               (list 4 1)
               (list 4 2)
               (list 4 3)
               (list 5 1)
               (list 5 2)
               (list 5 3)
               (list 5 4)
               (list 6 1)
               (list 6 2)
               (list 6 3)
               (list 6 4)
               (list 6 5)))

(check-equal? (sort (unique-pairs 6) list-less-than?) list1)
