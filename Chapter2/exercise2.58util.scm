
(define s '(x + (3 * (x + (y + 2)))))

(define a '(x + 3 * (x + y + 2)))

(define b '(x * y * z + w * q ))

(define d '(x * y * z + w * q + r * p * t + n + m))
;(check-equal? (deriv '(x + (3 * (x + (y + 2)))) 'x)
;              (prefix-deriv '(+ x (* 3 (+ x (+ y 2)))) 'x))

(define (infix-to-prefix exp)
   (if (list? (caddr exp))
      (append (cons (cadr exp) 
                    (if (pair?  exp)
                        (list (infix-to-prefix (car exp)))
                        (list (car exp))))
              (list (infix-to-prefix (caddr exp))))
      (list (cadr exp) (car exp) (caddr exp))))

(define (to-prefix exp)
  (if (eq? (car exp) '+) 
          (prefix-infix-to-prefix exp)
          (make-prefix exp '* 15)))


(define (prefix-infix-to-prefix exp)
   (display exp)
   (newline)
  (let ((left (cadr exp))
        (right (caddr exp)))
    (list '+ 
      (if (pair? left)
        (to-prefix left)
        left)
      (if (pair? right)
        (to-prefix right)
        right)
        )))

(define (make-prefix exp op count)
  (if (= 0 count) exp
    (let ((e (epsilon exp op)))
      (let ((left (car e))
            (right (cadr e)))
        (if (null? right)
          left
          (list op 
                (make-prefix left op (- count 1)) 
                (make-prefix right op (- count 1))))))))

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
                  (if (null? (cdddr exp)) (caddr exp) (cddr exp))
                  )
  ;          (list (append left (list (car exp))) (cddr exp))
            (ambit (list (append left (list (car exp))) (cdr exp)) op)))
        (list (append left exp) '()))

    ))

