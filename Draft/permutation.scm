(include "../utils.scm")
; algorithm "main" generates all possible permutations of all subset of the set

; (permutate '(()(1 2 3)) )
;  ()(123) -> (1)(2 3) -> (1 2)(3) -> (1 2 3)()
;                      -> (1 3)(2) -> (1 3 2)()
;
;          -> (2)(1 3) -> (2 1)(3) -> (2 1 3)()
;                      -> (2 3)(1) -> (2 3 1)()
;             
;          -> (3)(1 2) -> (3 1)(2) -> (3 1 2)()
;                      -> (3 2)(1) -> (3 2 1)()

; (main (1 2 3)) => (() (1) (2) (3) (1 2) (1 3) (2 1) (2 3) (3 1) (3 2) 
;                       (1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))


(define (perm first second constant)
;  (display first) (display second) (newline)
  (let ((null-first? (null? first))
        (null-second? (null? second)))
  (cond [null-second? '()]
        [else 
          (append 
            (list (append first (list (car second)))) 
            (perm first (cdr second) constant))]
        )))


(define (remove-sublist sublist list)
  (if (null? sublist)
      list
      (remove-sublist (remove (car sublist) sublist) (remove (car sublist) list))))

(define (permutate items)
  (let ((result (car items))
        (tail (cadr items)))
;      (display "=>") (display result) (display tail) (newline)
      (map (lambda (x) (cons x (list (remove-sublist x tail))))
           (perm result tail tail))))
  
(define (permutation items)
  (if (null? items)
      '()
    (append items (permutation (flatmap permutate items)))))

(define (main items)
  (map car (permutation (list (list '() items)))))











