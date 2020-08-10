;#lang scheme

(include "2.38.scm")
; see 2.39-test.scm

; Complete the following definitions of reverse
; in terms of fold-right and fold-left from


(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
  
(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))
