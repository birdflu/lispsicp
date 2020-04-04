#lang scheme

; The constructor make-center-percent takes
; a center and a percentage tolerance and produces
; the desired interval. The selector percent produces
; the percentage tolerance for a given interval.

(define (make-center-percent c p)
  (make-interval (- c (value c p)) (+ c (value c p))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (* (/ 100 (center i)) (width i)))

(define (value c p)
  (* (/ c 100) p))

(define (make-interval a b) (cons a b))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

(make-center-percent 4 25)
(make-center-percent 4 (percent (make-center-percent 4 25)))

