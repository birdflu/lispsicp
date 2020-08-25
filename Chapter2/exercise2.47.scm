;#lang scheme
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-vector frame)
  (car frame))

(define (edge1-vector frame)
  (cadr frame))

(define (edge2-vector frame)
  (caddr frame))

(define (origin-frame frame)
  (origin-vector frame))

(define (add-vectors v1 v2)
  (cons (+ (car v2) (car v1))
        (+ (cdr v2) (cdr v1))))
  
(define (edge1-frame frame)
   (add-vectors (origin-vector frame) (edge1-vector frame)))
  
(define (edge2-frame frame)
   (add-vectors (origin-vector frame) (edge2-vector frame)))

;(origin-frame (make-frame '(1 2) '(3 4) '(5 6)))
;(edge1-frame (make-frame '(1 2) '(3 4) '(5 6)))
;(edge2-frame (make-frame '(1 2) '(3 4) '(5 6)))
