#lang scheme

; By testing the signs of the endpoints of the intervals, it is
; possible to break mul-interval into nine cases, only one
; of which requires more than two multiplications.

https://github.com/birdflu/lispsicp/blob/master/Chapter2/2.11.png
https://github.com/birdflu/lispsicp/blob/master/Chapter2/2.11-2.png


(define (fast-mul-interval a b)
  (let ((_a (lower-bound a))
        (_b (lower-bound b))
        (a- (upper-bound a))
        (b- (upper-bound b)))
  (cond
    ((and (nonnegative-interval? a) (nonnegative-interval? b))
         (make-interval (* _a _b) (* a- b-)))
    ((and (nonnegative-interval? a) (has-interval-zero? b))
         (make-interval (* a- _b) (* a- b-)))
    ((and (nonnegative-interval? a) (nonpositive-interval? b))
         (make-interval (* a- _b) (* _a b-)))
    ((and (has-interval-zero? a) (nonnegative-interval? b))
         (make-interval (* _a b-) (* a- b-)))
    ((and (has-interval-zero? a) (has-interval-zero? b))
         (make-interval (min (* _a b-) (* a- _b)) (max (* _a _b) (* a- b-))))
    ((and (has-interval-zero? a) (nonpositive-interval? b))
         (make-interval (* a- _b) (* _a _b)))
    ((and (nonpositive-interval? a) (nonnegative-interval? b))
         (make-interval (* _a b-) (* a- _b)))
    ((and (nonpositive-interval? a) (has-interval-zero? b))
         (make-interval (* _a b-) (* _a _b)))
    ((and (nonpositive-interval? a) (nonpositive-interval? b))
         (make-interval (* a- b-) (* _a _b)))
    (else (error "EROR intervals " a b))
    )))

(define (nonpositive-interval? interval)
  (not (or  (has-interval-positive? interval) (has-interval-zero? interval))))

(define (nonnegative-interval? interval)
  (not (or  (has-interval-negative? interval) (has-interval-zero? interval))))

(define (has-interval-zero? interval)
  (if (<= 0 (* (lower-bound interval) (upper-bound interval))) #f #t))

(define (has-interval-negative? interval)
  (if (or (<= 0 (lower-bound interval)) (<= 0 (upper-bound interval))) #f #t))

(define (has-interval-positive? interval)
  (if (or (>= 0 (lower-bound interval)) (>= 0 (upper-bound interval))) #f #t))
 
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
  (cond ((has-interval-zero?  x) (error "Interval " x " has ZERO"))
        ((has-interval-zero?  y) (error "Interval " y " has ZERO"))
        (else (mul-interval
               x
               (make-interval (/ 1.0 (upper-bound y))
                              (/ 1.0 (lower-bound y)))))))
  
(define (test interval-a interval-b)
  (let ((a-mul-b (mul-interval interval-a interval-b))
        (a-fast-mul-b (fast-mul-interval interval-a interval-b))
        )
    (newline)
    (display "Intervals: ")
    (display interval-a)
    (display " and ")
    (display interval-b)
    (display ", mul ")
    (display a-mul-b)
    (display ", fast-mul: ")
    (display a-fast-mul-b)
    ))

(test (make-interval 2 8) (make-interval 4 10))
(test (make-interval 2 4) (make-interval -6 -2))
(test (make-interval 2 4) (make-interval -6 2))
(test (make-interval -2 8) (make-interval 4 10))
(test (make-interval -2 4) (make-interval -6 -2))
(test (make-interval -2 4) (make-interval -6 2))
(test (make-interval -4 -2) (make-interval -6 -2))
(test (make-interval -4 -2) (make-interval -2 6))
(test (make-interval -4 -2) (make-interval 2 6))

(test (make-interval 0 8) (make-interval 0 10))
(test (make-interval -8 0) (make-interval -10 0))
(test (make-interval -8 8) (make-interval -10 0))
(test (make-interval 0 0) (make-interval 0 0))
