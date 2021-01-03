; #lang scheme
(include "exercise2.3.2.scm")
(include "../Chapter3/exercise3.3.3-2.scm")
(include "../utils.scm")

; Modify the rational-arithmetic package to
; use generic operations, but change make-rat so that it does
; not attempt to reduce fractions to lowest terms. Test your
; system by calling make-rational on two polynomials to
; produce a rational function 

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
  ; (display "op:") ; (display op) (newline)
  ; (display "a:") ; (display a) (newline)
  (if (eq? '=zero? op)
      (apply-two-args op (list a))
      (let ((args-tail (cddr a)))
        (let ((args (if (eq? args-tail '())
                        (cons (car a) (cdr a))
                        (list (car a) (cadr a)))))
          ; (display "args=") ; (display args) (newline)
          ; (display "args-tail=") ; (display args-tail) (newline)
          (if (eq? args-tail '())
              (apply-two-args op args)
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

(define (raise-one-level obj)
  (define (raise-one-level-type obj tower)
    (if (eq? (car tower) (type-tag obj))
        (cadr tower)
        (raise-one-level-type obj (cdr tower))))
  (let ((type (type-tag obj)))
    (if (eq? type 'complex)
        obj
        (let ((raise-type
                (raise-one-level-type obj type-tower)))
          (let ((type->raise-type (get-coercion type raise-type)))
           ;  (display type) (newline)
           ;  (display raise-type) (newline)
           ;  (display type->raise-type) (newline)
            (type->raise-type obj))))))

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
;    (let ((g (gcd n d)))
;      (cons (/ n g) (/ d g))))
    (cons n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
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
;  (define (scheme-number->complex n)
;    (make-complex-from-real-imag (contents n) 0))
  (define (scheme-number->rational n)
    (make-rational (contents n) 1))
  (define (rational->complex n)
    (make-complex-from-real-imag (/ (cadr n) (cddr n)) 0))
;  (put-coercion 'scheme-number
;                'complex
;                scheme-number->complex)
  (put-coercion 'scheme-number
                'rational
                scheme-number->rational)
  (put-coercion 'rational
                'complex
                rational->complex)
  'done)

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (cadr p))
  (define (term-list p) (cddr p))
  ;  ⟨procedures same-variable? and variable? from section 2.3.2⟩
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (change-poly-sign p)
    (let ((changed-polynomial
            (make-polynomial
              (variable p)
              (change-sign (term-list p)))))
      (if (dense? p) (sparse->dense changed-polynomial) changed-polynomial)))
  (define (change-sign terms)
    (map
      (λ(term) (make-term (order term) (* -1 (coeff term))))
      (if (dense-terms? terms) (terms-dense->sparse terms) terms)))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (merge (quick-sort > car (mul-terms (term-list p1) (term-list p2)))))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
            (make-term (+ (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
            (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (terms-dense->sparse term-list)
    (define (transform term-list n)
      ;  (display term-list) (newline)
      (if (eq? term-list '())
          '()
          (if (= 0 (car term-list))
              (transform (cdr term-list) (+ n 1))
              (cons (list n (car term-list)) (transform (cdr term-list) (+ n 1))))))
    (transform (reverse term-list) 0))
  (define (terms-sparse->dense term-list)
    (map cadr (sparse->sparse-with-zeros (quick-sort > car term-list))))
  (define (sparse->sparse-with-zeros term-list)
    ; (display term-list) (newline)
    (if (eq? term-list '())
        '()
        (let ((1th-term (car term-list))
              (1th-order (caar term-list))
              (2th-term (if (eq? (cdr term-list) '()) '() (cadr term-list)))
              (2th-order (if (eq? (cdr term-list) '()) '() (caadr term-list)))
              (1th-tail (cdr term-list))
              (2th-tail (if (eq? (cdr term-list) '()) '() (cdr (cdr term-list)))))
          (cond ((and (eq? 1th-tail '()) (= 0 1th-order))
                 term-list)
                ((and (eq? 1th-tail '()) (not (= 0 1th-order)))
                 (sparse->sparse-with-zeros (list 1th-term '(0 0))))
                ((and (not (eq? 1th-tail '())) (not (= 1th-order (+ 1 2th-order))))
                 (sparse->sparse-with-zeros
                   (cons 1th-term
                         (cons (list (+ 1 2th-order) 0) '1th-tail))))
                (else (cons 1th-term
                            (sparse->sparse-with-zeros (cons 2th-term 2th-tail))))
                ))))
  (define (merge sorted-term-list)
    ; (display sorted-term-list) (newline)
    (if (empty? sorted-term-list)
        '()
        (if (empty? (cdr sorted-term-list))
            sorted-term-list
            (let ((1th-term (car sorted-term-list))
                  (1th-order (caar sorted-term-list))
                  (1th-coeff (cadar sorted-term-list))
                  (2th-term (if (eq? (cdr sorted-term-list) '()) '() (cadr sorted-term-list)))
                  (2th-order (if (eq? (cdr sorted-term-list) '()) '() (caadr sorted-term-list)))
                  (2th-coeff (if (eq? (cdr sorted-term-list) '()) '() (cadadr sorted-term-list)))
                  (1th-tail (cdr sorted-term-list))
                  (2th-tail (if (eq? (cdr sorted-term-list) '()) '() (cdr (cdr sorted-term-list)))))
              (if (equal? 1th-order 2th-order)
                  (merge (cons (list 1th-order (+ 1th-coeff 2th-coeff)) 2th-tail))
                  (cons 1th-term (merge 1th-tail)))))))
  (define (sparse-terms? terms) (pair? (car terms)))
  (define (sparse? p) (sparse-terms? (term-list p)))
  (define (dense-terms? terms) (not (sparse-terms? terms)))
  (define (dense? p) (not (sparse? p)))
  (define (sparse->dense p)
    (make-polynomial (variable p) (terms-sparse->dense (term-list p))))
  (define (dense->sparse p)
    (make-polynomial (variable p) (terms-dense->sparse (term-list p))))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (define (transf p1 p2 place)
    (cond ((and (sparse? p1) (sparse? p2) (eq? place 'p1)) (λ (x) x))
          ((and (sparse? p1) (sparse? p2) (eq? place 'p2)) (λ (x) x))
          ((and (sparse? p1) (sparse? p2) (eq? place 'p)) (λ (x) x))
          ((and (dense? p1) (dense? p2) (eq? place 'p1)) dense->sparse)
          ((and (dense? p1) (dense? p2) (eq? place 'p2)) dense->sparse)
          ((and (dense? p1) (dense? p2) (eq? place 'p)) sparse->dense)
          ((and (dense? p1) (sparse? p2) (eq? place 'p1)) dense->sparse)
          ((and (dense? p1) (sparse? p2) (eq? place 'p2)) (λ (x) x))
          ((and (dense? p1) (sparse? p2) (eq? place 'p)) sparse->dense)
          ((and (sparse? p1) (dense? p2) (eq? place 'p1)) (λ (x) x))
          ((and (sparse? p1) (dense? p2) (eq? place 'p2)) dense->sparse)
          ((and (sparse? p1) (dense? p2) (eq? place 'p)) (λ (x) x))
          (else '())))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((div (div-terms (term-list p1) (term-list p2))))
          (list (make-poly (variable p1) (car div))
                (make-poly (variable p1) (cadr div))))
        (error "Polys not in same var: DIV-POLY" (list p1 p2))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                        (div-terms
                          (add-terms
                            L1
                            (change-sign (mul-terms (list (make-term new-o new-c)) L2)))
                          L2)
                        ))
                  (cons (cons (make-term new-o new-c)
                              (car rest-of-result)) (cdr rest-of-result))
                  ))))))
  (put 'add '(polynomial polynomial)
       (λ (p1 p2)
          ((transf p1 p2 'p)
           (tag (add-poly ((transf p1 p2 'p1) p1) ((transf p1 p2 'p2) p2))))))
  (put 'sub '(polynomial polynomial)
       (λ (p1 p2)
          ((transf p1 p2 'p)
           (tag (add-poly ((transf p1 p2 'p1) p1) ((transf p1 p2 'p2) (change-sign p2)))))))
  (put 'mul '(polynomial polynomial)
       (λ (p1 p2)
          ((transf p1 p2 'p)
           (tag (mul-poly ((transf p1 p2 'p1) p1) ((transf p1 p2 'p2) p2))))))
  (put 'div '(polynomial polynomial)
       (λ (p1 p2)
          (let ((div (div-poly ((transf p1 p2 'p1) p1) ((transf p1 p2 'p2) p2))))
            (list ((transf p1 p2 'p) (tag (car div))) ((transf p1 p2 'p) (tag (cadr div)))))))
  (put '=zero? '(polynomial)
       (λ (p) (or
                (eq? (cddr p) '())
                (and (= (car (cdaddr p)) 0) (eq? (cdddr p) '())))))
  (put 'make 'polynomial
       (λ (var terms) (tag (make-poly var terms))))
  (put 'change-sign 'polynomial
       (λ (p) (change-poly-sign p)))
  'done)

 ; ordinal
 (install-scheme-number-package)
 
 ; rational
 (install-rational-package)
 
 ; complex
 (install-complex-package)
 
 ; type coercion
 (install-type-coercion-package)

 ; polynomial-package  
 (install-polynomial-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
 
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
