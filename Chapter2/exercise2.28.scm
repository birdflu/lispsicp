#lang scheme

; a procedure fringe takes as argument
; a tree (represented as a list) and returns a list whose
; elements are all the leaves of the tree arranged in left-to-right order.

(define (fringe x)
  (cond ((null? x) x)
       ((not (pair? (car x))) x )
       (else (append
              (fringe (car x))
              (fringe (cdr x))))))

(define (count-leaves x)
  (cond ((null? x) 0)
       ((not (pair? x)) 1)
       (else (+ (count-leaves (car x))
               (count-leaves (cdr x))))))

(define x (list (list 1 2) (list 3 4)))

(define z (list (list (list 1 2) 3) (list 4 5)))

(fringe x) ; (1 2 3 4)
(fringe (list x x)) ; (1 2 3 4 1 2 3 4)
(fringe (list (list x x) (list x x))) ; (1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4)

(fringe z)  ;(1 2 3 4 5)
