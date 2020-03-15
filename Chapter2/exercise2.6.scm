#lang scheme

; This representation is known as Church numerals and plus operation

(define id (λ(x) x))

(define zero (λ(f) (λ(x) x))) ; c0 = λf. λx. x;

(define (add-1 n)
  (λ(f) (λ(x) (f ((n f) x)))))

;(define one (add-1 zero)) =>
;(define one ((λ(f) (λ(x) (f ((zero f) x)))))) =>
;(define one ((λ(f) (λ(x) (f (((λ(f) (λ(x) x)) f) x)))))) =>
;(define one ((λ(f) (λ(x) (f ((λ(x) x) x)))))) =>
;(define one (λ(f) (λ(x) (f x))))

(define one (λ(f) (λ(x) (f x))))  ; c1 = λf. λx. f x;

;(define two (add-1 one)) =>
;(define two ((λ(f) (λ(x) (f ((n f) x)))) one)) =>
;(define two (λ(f) (λ(x) (f ((one f) x))))) =>
;(define two (λ(f) (λ(x) (f (((λ(f) (λ(x) (f x))) f) x))))) =>
;(define two (λ(f) (λ(x) (f ((λ(x) (f x)) x))))) =>
;(define two (λ(f) (λ(x) (f ((f x)))))) =>
(define two (λ(f) (λ(x) (f (f x))))) ; c2 = λf. λx. f (f x); 

; addition of Church numerals can be performed by a termplusthat takes
; two Church numerals, m and n, as arguments, and yields anotherChurch numeral—
; i.e., a function—that accepts arguments s and z, applies s iterated n times to z
; (by passing s and z as arguments to n), and then applies s iterated m more times
; to the result: plus = λm. λn. λs. λz. m s (n s z) (Benjamin C. Pierce Types and Programming Languages chapter 5.2, p.61)
; plus (m, n) = (λm. λn. λs. λz. m s (n s z)) m n =>
; plus (m, n) = (λn. λs. λz. m s (n s z)) n =>
; plus (m, n) = λs. λz. m s (n s z)


(define (plus m n)
      (λ(s)
        (λ(z) ((m s) ((n s) z))
          )))


(define (add m n)
  (λ(s) (λ(z)
                ((m s) ((n s) z)))))

(define inc (λ(x) (+ 1 x)))
 
;(add-1 zero)
;(add-1 (add-1 zero))
;(plus one two)
;
;(((add-1 zero) (lambda (x) (+ x 1))) 0)
;(((add-1 (add-1 zero)) (lambda (x) (+ x 1))) 0)
;
;(((add one zero) (lambda (x) (+ x 1))) 0)
;(((add two one) (lambda (x) (+ x 1))) 0)
;(((add two zero) (lambda (x) (+ x 1))) 0)

((one inc) 0)
((two inc) 0)

(((plus one two)
  inc) 0)


