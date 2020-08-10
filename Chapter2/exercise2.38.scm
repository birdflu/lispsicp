;#lang scheme

; see 2.38-test.scm

; The accumulate procedure is also known as
; fold-right, because it combines the first element of the sequence
; with the result of combining all the elements to the
; right. There is also a fold-left, which is similar to foldright,
; except that it combines elements working in the opposite
; direction

(define nil '())

(define (fold-right op initial sequence)
  (if (null? sequence)
     initial
     (op (car sequence)
        (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
       result
       (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))
