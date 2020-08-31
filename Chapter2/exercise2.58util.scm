;#lang scheme
(define s '(x + (3 * (x + (y + 2)))))

(define (infix-to-prefix exp)
  ; (display exp) 
  ; (newline)
   (if (list? (caddr exp))
      (append (cons (cadr exp) 
                    (if (pair? (car exp))
                        (list (infix-to-prefix (car exp)))
                        (list (car exp))))
              (list (infix-to-prefix (caddr exp))))
      (list (cadr exp) (car exp) (caddr exp))))


