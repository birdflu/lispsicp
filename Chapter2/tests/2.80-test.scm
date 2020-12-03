#lang scheme

(require rackunit)
(include "../exercise2.80.scm")

; ordinal
(install-scheme-number-package)

(check-equal? #f (=zero? 1))
(check-equal? #t (=zero? 0))

; rational
(install-rational-package)

(define d (make-rational 3 4))
(define f (make-rational 0 4))

(check-equal? #f (=zero? d))
(check-equal? #t (=zero? f))

; complex
(install-complex-package)

(define zero (make-complex-from-real-imag 0 0))  
(define n (make-complex-from-real-imag 0 1))
(define m (make-complex-from-real-imag 1 0))

(check-equal? #t (=zero? zero))
(check-equal? #f (=zero? n))
(check-equal? #f (=zero? m))

