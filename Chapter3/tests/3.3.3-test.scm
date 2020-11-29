#lang scheme
(require rackunit)

(include "../exercise3.3.3.scm")

(define tab (make-table))


(check-equal? (insert! 'a 1 tab) 'ok)
(check-equal? (insert! 'b 2 tab) 'ok)

(check-equal? (lookup 'a tab) 1)
(check-equal? (lookup 'b tab) 2)

