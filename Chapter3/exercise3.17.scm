#lang scheme
(include "store/work/sicp/lispsicp/Chapter3/exercise3.14.scm")

(define (count-pairs x)
  (display x)
  (display (if (not (pair? x)) "-\n" "+\n"))
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define y (cons 1 2))
(define z (list y y y))
(define z1 (list (cons 1 2) (cons 1 2) (cons 1 2)))
(define z2 (list (cons 1 2)  2 (cons 1 2) 4 (cons 1 2)))
(define x (cons y (cons y y)))

