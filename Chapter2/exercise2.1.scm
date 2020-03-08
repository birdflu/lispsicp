#lang scheme

(define (make-rat n d)
  (let ((numer (/ n (gcd n d)))
        (denom (/ d (gcd n d))))
    (if (and  (positive? numer) (negative? denom))
        (cons (* -1 numer) (* -1 denom))    
        (cons numer denom)
        )))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(print-rat (make-rat 6 9)) ; 2/3 
(print-rat (make-rat -6 9)) ; -2/3 
(print-rat (make-rat 6 -9)) ; -2/3 
(print-rat (make-rat -6 -9)) ; 2/3
(print-rat (make-rat 1 -2)) ; -1/2
(print-rat (make-rat 6 -9)) ; -2/3
