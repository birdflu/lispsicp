#lang scheme

; Implement a representation for rectangles in a plane.
; In terms of your constructors and selectors, we create procedures
; that compute the perimeter and the area of a given rectangle.
; Then implement a different representation for rectangles:
; rectangle-p is defined by points and
; rectangle-s is defined by segments

(define (rectangle-p left-up-point right-up-point left-down-point)
  (let ((right-down-point (make-point (x-point right-up-point)  (y-point left-down-point))))
   (cons left-up-point (cons right-up-point (cons left-down-point (cons right-down-point '())))))
  )

(define (rectangle-s up-segment down-segment)
  (let ((left-up-point (start-segment up-segment))
        (right-up-point (end-segment up-segment))
        (left-down-point (start-segment down-segment))
        (right-down-point (end-segment down-segment)))
    (cons left-up-point (cons right-up-point (cons left-down-point (cons right-down-point '())))))
  )

(define (rectangle-width-height rectangle)
  (let ((left-up-point (start-segment rectangle))
        (right-up-point (start-segment (end-segment rectangle)))
        (left-down-point (start-segment (end-segment (end-segment rectangle))))
        (right-down-point (start-segment (end-segment (end-segment (end-segment rectangle))))))
    (let ((width (abs (- (x-point right-down-point) (x-point left-down-point))))
          (height (abs (- (y-point left-up-point) (y-point left-down-point)))))
      (cons width height)
      )))

(define (rectangle-perimeter rectangle)
  (let ((width (car (rectangle-width-height rectangle)))
        (height (cdr (rectangle-width-height rectangle))))
    (* 2 (+ width height))
    ))

(define (rectangle-square rectangle)
  (let ((width (car (rectangle-width-height rectangle)))
        (height (cdr (rectangle-width-height rectangle))))
    (* width height)
    ))

(define (midpoint-segment segment)
  (make-point
   (average (x-point (start-segment segment)) (x-point (end-segment segment)))
   (average (y-point (start-segment segment)) (y-point (end-segment segment)))
   )
  )

(define abs
  (lambda (n)
    (if (< n 0)
        (- 0 n)
        n))) 
    
(define (average x y) (/ (+ x y) 2))

(define (make-segment start-point end-point) (cons start-point end-point))

(define (start-segment segment) (car segment))

(define (end-segment segment) (cdr segment))

(define (make-point x y) (cons x y))

(define (x-point point) (car point))

(define (y-point point) (cdr point))

(define rectangle (rectangle-s (make-segment (make-point 3 7) (make-point 6 7)) (make-segment (make-point 3 2) (make-point 6 2))))
;((3 . 7) (6 . 7) (3 . 2) (6 . 2))
(define rectangle2 (rectangle-s (make-segment (make-point 3 -7) (make-point 6 -7)) (make-segment (make-point 3 2) (make-point 6 2))))
;((3 . -7) (6 . -7) (3 . 2) (6 . 2))

(define rectangle3 (rectangle-p (make-point 3 2) (make-point 6 2) (make-point 3 -7)))

(start-segment rectangle) ;(3 . 7)
(start-segment (end-segment rectangle)) ; (6 . 7)
(start-segment (end-segment (end-segment rectangle))) ;(3 . 2)
(start-segment (end-segment (end-segment (end-segment rectangle)))) ;(6 . 2)
(newline)
(rectangle-perimeter rectangle) ; 16
(rectangle-square rectangle) ; 15
(newline)
(rectangle-perimeter rectangle2) ; 24
(rectangle-square rectangle2) ; 27
(newline)
(rectangle-perimeter rectangle3) ; 24
(rectangle-square rectangle3) ; 27
