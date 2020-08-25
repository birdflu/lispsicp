
#lang scheme

(include "../exercise2.47.scm")
(include "../../images.scm")

(require rackunit)

(check-equal? 
  (origin-frame (make-frame (make-vect .3 .3) (make-vect .0 .7) (make-vect .7 .7)))
  '(0.3 . 0.3))
(check-equal? 
  (edge1-frame (make-frame (make-vect .3 .3) (make-vect .0 .7) (make-vect .7 .7)))
  '(0.3 . 1.0))
(check-equal? 
  (edge2-frame (make-frame (make-vect .3 .3) (make-vect .0 .7) (make-vect .7 .7)))
  '(1.0 . 1.0))

