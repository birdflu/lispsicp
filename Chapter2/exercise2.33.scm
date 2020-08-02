#lang scheme

(include "2.2.3.1.scm")

; Fill in the missing expressions to complete
; the following definitions of some basic list-manipulation
; operations as accumulations
(define nil '())

(define (map p sequence)
  (accumulate (λ (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (λ (x y) (+ 1 y)) 0 sequence))

test-tree
(enumerate-tree test-tree)
(map square (enumerate-tree test-tree))
(append (list -1 0) (enumerate-tree test-tree))
(length (append (list -1 0) (enumerate-tree test-tree)))
