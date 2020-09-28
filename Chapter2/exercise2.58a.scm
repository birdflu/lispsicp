(include "exercise2.58util.scm")

; The variables are symbols. They are identified by the primitive
; predicate symbol?:
(define (variable? x) (symbol? x))

; Two variables are the same if the symbols representing them are
; eq?:
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

; Sums and products are constructed as lists:
; To simplyfy if both summands are numbers, make-sum will
; add them and return their sum. Also, if one of the summands is 0, then
; make-sum will return the other summand
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))


; 0 times anything is 0 and 1 times anything is the thing itself:
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

; The base is the second item of the sum list:
(define (base s) (cadr s))

; The exponent is the third item of the sum list:
(define (exponent s) (caddr s))

; A sum is a list whose first element is the symbol +:
(define (sum? x) (and (pair? x) (eq? (car x) '+)))

; A exponentiation is a list whose first element is the symbol **:
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))

; The addend is the second item of the sum list:
(define (addend s) (cadr s))

(define (augend s) (if (eq? '() (cdddr s))
                       (caddr s)
                       (list '+ (caddr s) (cadddr s))))

; A product is a list whose first element is the symbol *:
(define (product? x) (and (pair? x) (eq? (car x) '*)))

; The multiplier is the second item of the product list:
(define (multiplier p) (cadr p))

(define (multiplicand p) (if (eq? '() (cdddr p))
                             (caddr p)
                             (list '* (caddr p) (cadddr p))))

; Differentiation of any such expression
(define (prefix-deriv exp var)
;  (display exp)
;  (newline)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (prefix-deriv (addend exp) var)
                              (prefix-deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (prefix-deriv (multiplicand exp) var))
          (make-product (prefix-deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp) 
                       (make-product (make-exponentiation (base exp) 
                                                          (if (number? (exponent exp))
                                                              (- (exponent exp) 1)
                                                              (list '- (exponent exp) 1)))
                                     (prefix-deriv (base exp) var))))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (deriv exp var)
  (prefix-deriv (infix-to-prefix exp '+) var))
