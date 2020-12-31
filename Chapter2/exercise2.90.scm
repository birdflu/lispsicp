; #lang scheme
(include "exercise2.3.2.scm")
(include "exercise2.84.scm")
(include "../utils.scm")

; Suppose we want to have a polynomial system that is efficient
; for both sparse and dense polynomials.
; One way to do this is to allow both kinds of term-list repre-
; sentations in our system. To do this we
; must distinguish different types of term lists and make the
; operations on term lists generic. Redesign the polynomial
; system to implement this generalization.

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
(define (change-sign p)
  (let ((changed-polynomial
          (make-polynomial
            (variable p)
            (map
              (λ(term) (make-term (order term) (* -1 (coeff term))))
              (term-list (if (dense? p) (dense->sparse p) p))))))
    (if (dense? p) (sparse->dense changed-polynomial) changed-polynomial)))
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
(define (sparse? p) (pair? (car (term-list p))))
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
(put 'add '(polynomial polynomial)
     (λ (p1 p2)
        ((transf p1 p2 'p)
         (tag (add-poly ((transf p1 p2 'p1) p1) ((transf p1 p2 'p2) p2))))))
(put 'sub '(polynomial polynomial)
     (λ (p1 p2)
        ((transf p1 p2 'p)
         (tag (add-poly ((transf p1 p2 'p1) p1) ((transf p1 p2 'p2) (change-sign p2)))))))
;       (λ (p1 p2) (tag (add-poly p1 (change-sign p2)))))
(put 'mul '(polynomial polynomial)
     (λ (p1 p2)
        ((transf p1 p2 'p)
         (tag (mul-poly ((transf p1 p2 'p1) p1) ((transf p1 p2 'p2) p2))))))
(put '=zero? '(polynomial)
     (λ (p) (or
              (eq? (cddr p) '())
              (and (= (car (cdaddr p)) 0) (eq? (cdddr p) '())))))
(put 'make 'polynomial
     (λ (var terms) (tag (make-poly var terms))))
(put 'change-sign 'polynomial
     (λ (p) (change-sign p)))
'done)

(define (change-sign p)
  ((get 'change-sign 'polynomial) p))
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))


; {polynomial x {0 5} {1 1} {2 2} {3 3}}
; 3x^3 + 2x^2 + 1x^1 + 5
;(define a (make-polynomial 'x '((0 5) (1 1) (2 2) (3 3))))

; {polynomial x 3 2 1 5}
;(define b (make-polynomial 'x '(3 2 1 5)))
