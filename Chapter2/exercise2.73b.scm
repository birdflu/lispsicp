; Symbolic differentiation:
(include "../Chapter3/exercise3.3.3-2.scm")

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;(define (make-sum a1 a2) (list '+ a1 a2))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))


(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (addend s) (car s))
(define (augend s) (cadr s))

(put 'deriv '+ (lambda (exp var)
                 (make-sum
                   (deriv (addend exp) var)
                   (deriv (augend exp) var))))


