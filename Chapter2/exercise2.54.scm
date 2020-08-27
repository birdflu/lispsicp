; Define equal? recursively in terms of the basic eq? 
; equality of symbols by saying that a and b are equal? 
; if they are both symbols and the symbols are eq?, 
; or if they are both lists such that (car a) is equal? 
; to (car b) and (cdr a) is equal? to (cdr b).
; Using this idea, implement equal? as a procedure.

(define (equal? x y)
  
   ; (display x) (display " ") (display y)
   ; (newline)
   (if (and (null? x) (null? y))
            #t
            (cond ((and (null? x) (not (null? y))) #f)
                  ((and (not (null? x)) (null? y)) #f)
                  ((and (list? x) (list? y)) 
                   (if (and (equal? (car x) (car y))
                       (equal? (cdr x) (cdr y))) 
                       #t
                       #f))
                  ((and (not (list? x)) (not (list? y))) (eq? x y))
                  (else #f))))

