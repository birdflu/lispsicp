#lang scheme

(require rackunit)
(include "../exercise2.79.scm")

; ordinal
(install-scheme-number-package)

(check-equal? #t (equ? (add 1 3) (add 3 1)))
(check-equal? #t (equ? (mul 1 3) (mul 3 1)))
(check-equal? #f (equ? 1 3))

; rational
(install-rational-package)

(define d (make-rational 3 4))
(define f (make-rational -1 4))

(check-equal? #t (equ? (add d f) (add f d)))
(check-equal? #t (equ? (mul d f) (mul f d)))
(check-equal? #f (equ? d f))

; complex
(install-complex-package)

(define n (make-complex-from-real-imag 5 2))  
(define m (make-complex-from-real-imag 2 1))

(check-equal? #t (equ? (add m n) (add n m)))
(check-equal? #t (equ? (mul m n) (mul n m)))
(check-equal? #f (equ? m n))

