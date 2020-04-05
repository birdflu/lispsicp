#lang scheme

; Show that under the assumption of small percentage tolerances there is a 
; simple formula for the approximate percentage tolerance of the product of two 
; intervals in terms of the tolerances of the factors. You may simplify the 
; problem by assuming that all numbers are positive.
; --------------------------------------------------

; If a = [_a,a-], b = [_b,b-] and  _a, a-, _b, b- are positive, then a*b = [_a_b, a-b-]
; Let a = [Ca*(1 - Pa), Ca*(1 + Pa)], b = [Cb*(1 - Pb), Cb*(1 + Pb)], where  Ci - center of interval i and Pi - percentage tolerances
; 
; [Ca*(1 - Pa), Ca*(1 + Pa)] * [Cb*(1 - Pb), Cb*(1 + Pb)]
; = [Ca*Cb*(1 - (Pa + Pb - Pa*Pb)), Ca*Cb*(1 + (Pa + Pb + Pa*Pb))]
;  Pa*Pb -> 0 
; 
; So percentage in this case can be expressed as Pa + Pb.

(define (make-center-percent c p)
  (make-interval (- c (value c p)) (+ c (value c p))))

(define (percent i)
  (* (/ 100 (center i)) (width i)))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (value c p)
  (* (/ c 100) p))

(define (fast-mul-interval a b)
  (let ((_a (lower-bound a))
        (_b (lower-bound b))
        (a- (upper-bound a))
        (b- (upper-bound b)))
    (cond
      ((and (nonnegative-interval? a) (nonnegative-interval? b))
       (make-interval (* _a _b) (* a- b-)))
      (else (error "EROR intervals " a b))
      )))

(define (nonnegative-interval? interval)
  (not (or  (has-interval-negative? interval) (has-interval-zero? interval))))

(define (has-interval-zero? interval)
  (if (<= 0 (* (lower-bound interval) (upper-bound interval))) #f #t))

(define (has-interval-negative? interval)
  (if (or (<= 0 (lower-bound interval)) (<= 0 (upper-bound interval))) #f #t))

(define (make-interval a b) (cons a b))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

(define (test)
  (let ((Ca 4)
        (Cb 6)
        (Pa 0.001)
        (Pb 0.003))
    (define a (make-center-percent Ca Pa))
    (define b (make-center-percent Cb Pb))

    (display "Pa =") (display (percent a))
    (newline)
    (display "Pb =") (display (percent b))
    (newline)
    (display "Pa*b =") (display (percent  (fast-mul-interval a b)))
    (newline)
    (display "Pa*b = Pa + Pb = ") (display (+ Pa Pb))
    ))

(test)

