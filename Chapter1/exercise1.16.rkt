#lang scheme
(define (fast-expt b n )
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b n a)
  (if (= n 0)
      a
      (if (even? n) (expt-iter (square b) (/ n 2) a)
          (expt-iter b (- n 1) (* a b))
)))

(display (fast-expt 5 10))
