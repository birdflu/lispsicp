#lang scheme

; The width of an interval is half of the difference
; between its upper and lower bounds. The width is a
; measure of the uncertainty of the number specified by the
; interval. For some arithmetic operations the width of the
; result of combining two intervals is a function only of the
; widths of the argument intervals, whereas for others the
; width of the combination is not a function of the widths of
; the argument intervals. Show that the width of the sum (or
; difference) of two intervals is a function only of the widths
; of the intervals being added (or subtracted). Give examples
; to show that this is not true for multiplication or division

; https://en.wikipedia.org/wiki/Interval_arithmetic
; [a,b] + [c,d] = [a + c, b + d]
; [a,b] - [c,d] = [a - d, b - c]
; [a,b] × [c,d] = [min (ac, ad, bc, bd), max (ac, ad, bc, bd)]
; [a,b] / [c,d] = [min (a/c, a/d, b/c, b/d), max (a/c, a/d, b/c, b/d)]

(define (radius-interval interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))
 
(define (make-interval a b) (cons a b))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (test interval-a interval-b)
  (let ((radius-a (radius-interval interval-a))
        (radius-b (radius-interval interval-b))
        (a-add-b (add-interval interval-a interval-b))
        (a-sub-b (sub-interval interval-a interval-b))
        (a-mul-b (mul-interval interval-a interval-b))
        (a-div-b (div-interval interval-a interval-b))
        )
  (newline)
  (display "Intervals: ")
  (display interval-a)
  (display " with radius ")
  (display radius-a)
  (display " and ")
  (display interval-b)
  (display " with radius ")
  (display radius-b)
  (display ", sum: ")
  (display (add-interval interval-a interval-b))
  (display ", sub: ")
  (display (sub-interval interval-a interval-b))
  (newline)
  (display "Is the radius of the interval sum equal to the sum of radiuses each interval? Answer: ")
  (display (radius-interval a-add-b))
  (display " = ")
  (display (+ radius-a radius-b))
  (display " (yes) ")
  (newline)
  (display "Is the radius of the interval sub equal to the sub of radiuses each interval? Answer: ")
  (display (radius-interval a-sub-b))
  (display " = ")
  (display (- radius-a radius-b))
  (display " (no) ")
  (newline)
  (display "Is the radius of the interval sub equal to the sum of radiuses each interval? Answer: ")
  (display (radius-interval a-sub-b))
  (display " = ")
  (display (+ radius-a radius-b))
  (display " (yes) ")
  (newline)
  (display "Is the radius of the interval multiply equal to the multiply of radiuses each interval? Answer: ")
  (display (radius-interval a-mul-b))
  (display " = ")
  (display (* radius-a radius-b))
  (display " (no) ")
  (newline)
  (display "Is the radius of the interval divide equal to the divide of radiuses each interval? Answer: ")
  (display (radius-interval a-div-b))
  (display " = ")
  (display (/ radius-a radius-b))
  (display " (no) ")
  (newline)
  ))

(test (make-interval 2 8) (make-interval 4 10))
(test (make-interval -2 4) (make-interval -6 2))
