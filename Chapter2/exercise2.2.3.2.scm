;#lang scheme

; see 2.2.3.2-test.scm

; Given a positive integer n, find all ordered pairs of distinct positive
; integers i and j, where 1 < j < i < n

; Then filter this sequence of pairs to find those whose sum is prime. The
; filter predicate is called for each element of the sequence; its argument is
; a pair and it must extract the integers from the pair. 

(include "../utils.scm")
(include "../Chapter1/1.22.rkt")

; generate all pairs
(define (generate-pairs init n)
  (accumulate
   append nil (map (λ (i)
                     (map (λ (j) (list i j))
                         (enumerate-interval init (- i 1))))
                  (enumerate-interval init n))))

(define (generate-pairs-2 j n)
  (if (= n j)
     nil
     (append
      (map (λ (i) (list i j)) (enumerate-interval (+ 1 j) n))
      (generate-pairs-2 (+ j 1) n)
      )))

(define (generate-pairs-3 init n)
  (flatmap (λ (i)
             (map (λ (j) (list i j))
                 (enumerate-interval init (- i 1))))
          (enumerate-interval init n)))
; pair sum
(define (pair-amount pair)
  (+ (car pair) (cadr pair)))
  
; predicate for primed filter 
(define (prime-sum? pair)
  (prime? (pair-amount pair)))

; for each pair (i; j) that
; passes through the filter, produce the triple (i; j; i + j).

(define (prime-sum-pairs n)
  (map (λ(x) (append x (list (pair-amount x))))
      (filter prime-sum? (generate-pairs-3 1 n))))

(define (permutations s)
  (if (null? s) 
     (list nil)
     (flatmap (λ(x)
                (map (λ (p) (cons x p)) (permutations (remove x s))))
             s)))
