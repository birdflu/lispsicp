(define s0 '((x * y) * (x + 3)))

(define s1 '(x + (3 * (x + (y + 2)))))

(define s2 '(x + 3 * (x + y + 2)))

(define s3 '(x * y * z + w * q ))

(define s4 '(x * y * z + w * q + r * p * t + n + m))

(define s5 '(x * (y + (z + t * (l + m)))))

(define s6 '(((x + b) + y) + c + (q + a)))

(define s7 '(((x + b) * y) + c * (q * a) + d))

(define a1 '(5 + 6 * 6 + 2 * (4 * 1 + 1 * (0 + 0)))) ; 49

(define a2 '(2 * (4 * 1 + 1 * (0 + 0)))) ; 8

(define a3 '(2 + 2 * (3 + 1) * 3 + (2 * (3 + 1) * 2 + 0) * 2)) ; 2 + 24  + 32 = 58

;(define (infix-to-prefix exp)
;   (if (list? (caddr exp))
;      (append (cons (cadr exp) 
;                    (if (pair?  exp)
;                        (list (infix-to-prefix (car exp)))
;                        (list (car exp))))
;              (list (infix-to-prefix (caddr exp))))
;      (list (cadr exp) (car exp) (caddr exp))))

(define (change-operation op)
 (if (eq? op '*) '+ '*))

(define (infix-to-prefix exp op)
;  (display (list (list "exp=" exp) (list "op=" op) (list "count= "  count)))
;  (newline)
  (let ((e (epsilon exp op)))
    (let ((left (car e))
          (right (cadr e)))
      (if (null? right)
        (if (not (pair? left)) 
          left
          (infix-to-prefix left (change-operation op)))
        (list op 
              (infix-to-prefix left '+) 
              (infix-to-prefix right '+))))))

(define (epsilon exp op)
  (ambit (list '() exp) op)) 

(define (ambit s op)
  ;(display s)
  ;(newline)
  (let ((exp (cadr s))
       (left (car s)))
     (if (pair? exp)
        (if (null? (cdr exp))
          (list (append left exp) '())
          (if (eq? op (cadr exp))
            (list (append left 
                        (if (null? left) (car exp) (list (car exp)))) 
                  (if (null? (cdddr exp)) (caddr exp) (cddr exp)))
            (ambit (list (append left (list (car exp))) (cdr exp)) op)))
        (list (append left exp) '()))))




