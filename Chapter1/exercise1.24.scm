#lang scheme

;Modify the timed-prime-test procedure of
;Exercise 1.22 to use fast-prime? (the Fermat method)
(define (search-for-primes number-from prime-count) 
  (if (> prime-count 0)
      (if (timed-prime-test  number-from)
          (search-for-primes (+ number-from 1) (- prime-count 1)) 
          (search-for-primes (+ number-from 1) prime-count))
      false))

(define (timed-prime-test n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
      (report-prime n (- (current-milliseconds) start-time))
      false))

(define (report-prime n elapsed-time)
  (display n)
  (display " takes ")
  (display elapsed-time)
  (display " ms ")
  (newline))


(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m)) m))))

(define (square a)
  (* a a))

(search-for-primes 1000000000 3)
