#lang scheme

(include "../exercise2.48.scm")
(require rackunit)

(check-equal? (end-segment (make-segment (make-vect .1 .9) (make-vect .5 .8)))
	      '(0.5 . 0.8))

(check-equal? (start-segment (make-segment (make-vect .1 .9) (make-vect .5 .8)))
	      '(0.1 . 0.9))

