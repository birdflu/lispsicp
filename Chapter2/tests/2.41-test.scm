#lang scheme

(include "../2.41.scm")
(require rackunit)

(define result (list (list 1 2 5) (list 1 3 4)))

(check-equal? (sort (unique-triples-sum-of 6 8) list-less-than?) result)
