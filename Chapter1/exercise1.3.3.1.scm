#lang scheme

; The half-interval method is a simple but powerful technique for finding
; roots of an equation f (x) = 0, where f is a continuous function. The
; idea is that, if we are given points a and b such that f (a) < 0 < f (b),
; then f must have at least one zero between a and b. To locate a zero,
; let x be the average of a and b, and compute f (x). If f (x) > 0, then
; f must have a zero between a and x. If f (x) < 0, then f must have a
; zero between x and b. Continuing in this way, we can identify smaller
; and smaller intervals on which f must have a zero. When we reach a
; point where the interval is small enough, the process stops.
; search is awkward to use directly, because we can accidentally give it
; points at which f ’s values do not have the required sign, in which case
; we get a wrong answer. Instead we will use search via the following
; procedure, which checks to see which of the endpoints has a negative
; function value and which has a positive value, and calls the search procedure
; accordingly. If the function has the same sign on the two given
; points, the half-interval method cannot be used, in which case the procedure
; signals an error.

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define (search f neg pos)
  (let ((midpoint (average neg pos)))
    (if (close? neg pos)
        midpoint
        (let ((testValue (f midpoint)))
          (cond ((positive? testValue) (search f neg midpoint))
                ((negative? testValue) (search f midpoint pos))
                (else midpoint))))))

(define (average x y)
  (/ (+ x y) 2))

(define (close? x y) (< (abs (- x y)) 0.001))

(half-interval-method cos 0 3.14)

(half-interval-method (lambda (x) (+ (* x x) (* 2 x) 0.1))
-0.1
2.0)
