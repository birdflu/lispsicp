(include "exercise3.1.1.scm")

; An accumulator is a procedure that is called
; repeatedly with a single numeric argument and accumulates 
; its arguments into a sum. Each time it is called, it
; returns the currently accumulated sum. Write a procedure
; make-accumulator that generates accumulators, each maintaining 
; an independent sum.


(define (make-accumulator balance)
  (define acc (make-account balance))
  (λ (amount) ((acc 'deposit) amount)))


;(define A (make-accumulator 5))
;(A 10)
;15
;(A 10)
;25

