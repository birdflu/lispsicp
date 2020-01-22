#lang scheme

; Procedure search-for-primes checks the primality of
; consecutive odd integers in a specified range.
(define (search-for-primes number-from prime-count) 
  (if (> prime-count 0)
      (if (timed-prime-test  number-from)
          (search-for-primes (+ number-from 1) (- prime-count 1)) 
          (search-for-primes (+ number-from 1) prime-count))
      false))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

; The timed-prime-test procedure, when called with an integer n,
; prints n and checks to see if n is prime.
(define (timed-prime-test n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (current-milliseconds) start-time))
      false))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square a)
  (* a a))

(define (report-prime n elapsed-time)
  (display n)
  (display " takes ")
  (display elapsed-time)
  (display " ms ")
  (newline))

(search-for-primes 1000000000 3)
