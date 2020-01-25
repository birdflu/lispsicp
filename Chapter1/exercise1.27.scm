#lang scheme

;Demonstrate that the Carmichael numbers really do fool the Fermat test.
;That is, a procedure takes an integer n and tests whether
;a^n is congruent to a modulo n for every a < n

(define (prime-carmichael? n)
  (fullscan-prime? n (- n 1)))


(define (fullscan-prime? n a)
  (cond ((= a 0) true)
        ((= (expmod a n n) a) (fullscan-prime? n (- a 1)))
        (else false)))


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

(prime-carmichael? 561)
(prime-carmichael? 1105)
(prime-carmichael? 1729)
(prime-carmichael? 2465)
(prime-carmichael? 2821)
(prime-carmichael? 6601)
