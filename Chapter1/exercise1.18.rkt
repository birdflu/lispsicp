#lang scheme

;Using the results of Exercise 1.16 and Exercise 1.17,
;devise a procedure that generates an iterative process
;for multiplying two integers in terms of adding, doubling,
;and halving and uses a logarithmic number of steps.

(define (* b n)
  (*-iter b n 0))

(define (*-iter b n a)
  (if (= n 0)
      a
      (if (even? n) (double (*-iter b (halve n) a))
          (+ b (*-iter b (- n 1) a )
))))

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))


(define (even? n)
  (= (remainder n 2) 0))


(display (* 3 5))
