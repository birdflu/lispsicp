#lang scheme

(include "../2.46.scm")
(require rackunit)

(check-equal? (make-vect 1 9) '(1 . 9))
(check-equal? (add-vect (make-vect 1 9) (make-vect 1 9) (make-vect 1 9)) '(3 . 27))
(check-equal? (scale-vect  3 (make-vect 1 9)) '(3 . 27))
