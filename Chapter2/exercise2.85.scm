; #lang scheme
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

(define (apply-generic op a)
  ; (newline)
  ; (display "op:") (display op) (newline)
  ; (display "a:") (display a) (newline)
  (if (eq? '=zero? op)
      (apply-two-args op (list a))
      (let ((args-tail (cddr a)))
        (let ((args (if (eq? args-tail '())
                        (cons (car a) (cdr a))
                        (list (car a) (cadr a)))))
          ; (display "args=") ; (display args) (newline)
          ; (display "args-tail=") ; (display args-tail) (newline)
          (if (eq? args-tail '())
              (if (eq? op 'equ?)
                  (apply-two-args op args)
                  (drop (apply-two-args op args)))
              (apply-generic op (cons (apply-generic op args) args-tail)))))))

(define (apply-two-args op args)
  ; (display "apply-two-args=") ; (display args) (newline)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      ; (display proc) (newline)
      ; (display "type-tags=") ; (display type-tags) (newline)
      (if proc
          ;(apply proc (map contents args))
          (apply proc args)
          (coerce-and-apply op args type-tags)
          ))))

(define type-tower (list 'scheme-number 'rational 'complex))

(define (lower? type1 type2)
  (define (get-first type1 type2 tower)
    (cond ((eq? (car tower) type1) type1)
          ((eq? (car tower) type2) type2)
          ((get-first type1 type2 (cdr tower)))))
  (and (eq? type1 (get-first type1 type2 type-tower)) (not (eq? type1 type2))))

(define (change-one-level direction obj)
  (define (raise-one-level-type obj tower)
    (if (eq? (car tower) (type-tag obj))
        (cadr tower)
        (raise-one-level-type obj (cdr tower))))
  (let ((type (type-tag obj))
        (top-type (if (eq? direction 'up) 'complex 'scheme-number))
        (tower (if (eq? direction 'up) type-tower (reverse type-tower))))
    (if (eq? type top-type)
        obj
        (let ((raise-type
                (raise-one-level-type obj tower)))
          (let ((type->raise-type (get-coercion type raise-type)))
           ;  (display type) (newline)
           ;  (display raise-type) (newline)
           ;  (display type->raise-type) (newline)
            (type->raise-type obj))))))

(define (raise-one-level obj)
  (change-one-level 'up obj))

(define (drop-one-level obj)
  (change-one-level 'down obj))

(define (drop obj)
  ; (display "=") (display obj) (newline)

  (if (eq? (type-tag obj) 'scheme-number) 
      obj
      (if (equ? obj (raise-one-level (drop-one-level obj)))
          (drop (drop-one-level obj))
          obj)))

(define (coerce-and-apply op args type-tags)
  ; (display args) (newline)
  ; (display type-tags) (newline)
  
  (let ((type1 (car type-tags))
        (type2 (cadr type-tags))
        (a1 (car args))
        (a2 (cadr args)))
    (cond [(lower? type1 type2)
           (apply-generic op (list (raise-one-level a1) a2))]
          [(lower? type2 type1) 
           (apply-generic op (list a1 (raise-one-level a2)))]
          [(not (eq? type1 type2)) 
           (error "No method for these types . " (list op type-tags))]
          [else (error "type raising isn't need . " (list op type-tags))])))

(define (attach-tag type-tag contents)
  (if (equal? 'scheme-number type-tag)
      contents
      (cons type-tag contents)))

(define (add . args) (apply-generic 'add args))
(define (sub . args) (apply-generic 'sub args))
(define (mul . args) (apply-generic 'mul args))
(define (div . args) (apply-generic 'div args))
(define (equ? . args) (apply-generic 'equ? args))
(define (=zero? x) (apply-generic '=zero? x))
(define (exp . args) (apply-generic 'exp args))

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
(define (install-type-coercion-package)
  (define (scheme-number->rational n)
    (make-rational (contents n) 1))
  (define (rational->scheme-number n)
    (cadr n))
  (define (rational->complex n)
    (make-complex-from-real-imag (/ (cadr n) (cddr n)) 0))
  (define (complex->rational n)
    (make-rational (caddr n) 1))
  (put-coercion 'scheme-number
                'rational
                scheme-number->rational)
  (put-coercion 'rational
                'scheme-number
                rational->scheme-number)
  (put-coercion 'rational
                'complex
                rational->complex)
  (put-coercion 'complex
                'rational
                complex->rational)
 'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;  ; ordinal
;  (install-scheme-number-package)
;  
;  ; rational
;  (install-rational-package)
;  
;  ; complex
;  (install-complex-package)
;  
;  ; type coercion
;  (install-type-coercion-package)

