#lang scheme

(require rackunit)
(include "../2.38.scm")

(define list1 (list 1 2 3))
(define list2 (list (list 1) (list 2) (list 3)))

; not commutativity
(check-equal? (fold-right / 1 list1) (/ 3 2))
(check-equal? (fold-left / 1 list1) (/ 1 6))
(check-equal? (fold-right list nil list1) (list 1 (list 2 (list 3 nil))))
(check-equal? (fold-left list nil list1) (list (list (list nil 1) 2) 3))

; commutativity
(check-equal? (fold-right * 1 list1) (fold-left * 1 list1))
(check-equal? (fold-right + 1 list1) (fold-left + 1 list1))
(check-equal? (fold-right append nil list2) (fold-left append nil list2))
