;#lang scheme
; Use segments->painter to define the
; some primitive painters

(include "../images.scm")
(include "exercise2.47.scm")

(define origin (make-vect .1 .1))
(define edge1 (make-vect .0 .2))
(define edge2 (make-vect .2 .2))
(define v4 (make-vect .3 .6))
(define v5 (make-vect .4 .7))

(define frame (make-frame origin edge1 edge2))

;a. The painter that draws the outline of the designated
;frame.

(define (designated-frame frame)
  (segments->painter
   (let ((a (make-segment (origin-frame frame) (edge1-frame frame)))
         (b (make-segment (origin-frame frame) (edge2-frame frame)))
         (c (make-segment (edge1-frame frame) (add-vectors (edge1-frame frame) (edge2-vector frame))))
         (d (make-segment (edge2-frame frame) (add-vectors (edge1-vector frame) (edge2-frame frame)))))
     (list a b c d))))

;b. The painter that draws an “X” by connecting opposite
;corners of the frame.

(define (X-frame frame)
  (segments->painter
   (list (make-segment (origin-frame frame) (add-vectors (edge1-frame frame) (edge2-vector frame)))
         (make-segment (edge1-frame frame) (edge2-frame frame))
         )))

;c. The painter that draws a diamond shape by connect-
;ing the midpoints of the sides of the frame.

(define (diamond-frame frame)
  (segments->painter
   (let ((a (make-segment (origin-frame frame) (edge1-frame frame)))
         (b (make-segment (origin-frame frame) (edge2-frame frame)))
         (c (make-segment (edge1-frame frame) (add-vectors (edge1-frame frame) (edge2-vector frame))))
         (d (make-segment (edge2-frame frame) (add-vectors (edge1-vector frame) (edge2-frame frame)))))
     (list (make-segment (segment-middle a) (segment-middle b))
           (make-segment (segment-middle b) (segment-middle d))
           (make-segment (segment-middle d) (segment-middle c))
           (make-segment (segment-middle c) (segment-middle a))
           ))))

;d. The wave painter.