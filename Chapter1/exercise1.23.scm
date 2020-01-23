#lang scheme

; Procedure search-for-primes checks the primality of
; consecutive odd integers in a specified range.
; It is optimized by next-procedure for use 2,3,5,7,9
; instead 2,3,4,5,6
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
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

(define (square n)
  (* n n))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (timed-prime-test n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (current-milliseconds) start-time))
      false))

(define (report-prime n elapsed-time)
  (display n)
  (display " takes ")
  (display elapsed-time)
  (display " ms ")
  (newline))

(search-for-primes 10000000000 3)
