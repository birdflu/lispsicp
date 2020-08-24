#lang scheme
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

;(origin-frame (make-frame '(1 2) '(3 4) '(5 6)))
;(edge1-frame (make-frame '(1 2) '(3 4) '(5 6)))
;(edge2-frame (make-frame '(1 2) '(3 4) '(5 6)))
