(include "../Chapter3/exercise3.3.3-2.scm")

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (if (number? datum)
          'scheme-number
          (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (if (pair? datum)
      (cddr datum)
      (if (number? datum)
          datum
          (error "Bad tagged datum: CONTENTS" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          ;(apply proc (map contents args))
          (apply proc args)
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                          (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                          (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))


(define (attach-tag type-tag contents)
  (if (equal? 'scheme-number type-tag)
      contents
      (cons type-tag contents)))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (exp x y) (apply-generic 'exp x y))

(define (square x) (* x x))
(define (real-part c) ((get (cadr c) 'real-part) c))
(define (imag-part c) ((get (cadr c) 'imag-part) c))
(define (magnitude c) ((get (cadr c) 'magnitude) c))
(define (angle c) ((get (cadr c) 'angle) c))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (λ (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (λ (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (λ (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (λ (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (λ (x y) (eq? x y)))
  (put '=zero? '(scheme-number)
       (λ (x) (= x 0)))
  (put 'exp '(scheme-number scheme-number)
       (λ (x y) (tag (expt x y))))
  (put 'make 'scheme-number (λ (x) (tag x)))
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (cadr x))
  (define (denom x) (cddr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ-rational? x y)
    (and (= (numer x) (numer y)) (= (denom x) (denom y))))
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (λ (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (λ (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (λ (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (λ (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (λ (x y) (equ-rational? x y)))
  (put '=zero? '(rational)
       (λ (x) (and (= (numer x) 0) (not (= (denom x) 0)))))
  (put 'make 'rational
       (λ (x y) (tag (make-rat x y))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define zero (cons 'complex (cons 'rectangular (cons 0 0))))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ-complex? z1 z2)
    (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2))))
  
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  
  (put 'rectangular 'real-part caddr)
  (put 'rectangular 'imag-part cdddr)
  (put 'rectangular 'magnitude (λ (z)
                                  (sqrt (+ (square (real-part z))
                                           (square (imag-part z))))))
  (put 'rectangular 'angle (λ (z) (atan (imag-part z) (real-part z))))
  
  (put 'polar 'real-part (λ (z) (* (magnitude z) (cos (angle z)))))
  (put 'polar 'imag-part (λ (z) (* (magnitude z) (sin (angle z)))))
  (put 'polar 'magnitude caddr)
  (put 'polar 'angle cdddr)
  > (install-complex-package )
'done
> (exp 2 3)
8
> (exp (make- 3)
Display all 132 possibilities? (y or n) 
> (exp (make-complex-from-real-imag 2 0) (make-complex-from-real-imag 3 0))
  (put 'add '(complex complex)
       (λ (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (λ (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (λ (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (λ (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (λ (z1 z2) (equ-complex? z1 z2)))
  (put '=zero? '(complex)
       (λ (x) (equ-complex? x zero)))
  
  (put 'make-from-real-imag 'complex
       (λ (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (λ (r a) (tag (make-from-mag-ang r a))))
  
  (put 'make-from-real-imag 'rectangular
       (λ (x y) (attach-tag 'rectangular (cons x y))))
  (put 'make-from-mag-ang 'rectangular
       (λ (r a) (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a))))))
  (put 'make-from-real-imag 'polar
       (λ (x y) (attach-tag 'polar (cons (sqrt (+ (square x) (square y)))
                                         (atan y x)))))
  (put 'make-from-mag-ang 'polar
       (λ (r a) (attach-tag 'polar (cons r a))))
  
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

; type coercion
(define (type-coercion-package)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  (define (scheme-number->scheme-number n) n)
  (define (complex->complex z) z)
  
  (put-coercion 'scheme-number
                'complex
                scheme-number->complex)
  (put-coercion 'scheme-number
                'scheme-number
                scheme-number->scheme-number)
  (put-coercion 'complex 'complex complex->complex)
  
  'done)


(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; > (install-scheme-number-package  )
; 'done
; > (install-complex-package )
; 'done
; > (exp 2 3)
; 8
; > (exp (make-complex-from-real-imag 2 0) (make-complex-from-real-imag 3 0))
; ; No method for these types {exp {complex complex}} [,bt for context]
