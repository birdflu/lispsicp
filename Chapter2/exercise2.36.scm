#lang scheme
(include "2.2.2.1.rkt")
(include "2.2.3.1.rkt")

; Redefine count-leaves from Section 2.2.2
; as an accumulation

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
     nil
     (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

; (accumulate-n + 0 s) ; (22 26 30)
