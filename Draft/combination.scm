(include "../utils.scm")
; algorithm "main" generates all possible permutations of all subset of the set

; (combinatation '(()(1 2 3)) )
; ()(1 2 3) -> (1)(2 3) -> (1 2)(3) -> (1 2 3)()
;                       -> (1 3)() 
;
;           -> (2)(3) -> (2 3)() 
;             
;           -> (3)() 

; (main (1 2 3)) => (() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))

(define (combinate items)
  ;  (display first) (display second) (newline)
  (let ((first (car items))
        (second (cadr items)))
    (let ((null-first? (null? first))
          (null-second? (null? second)))
    (cond [null-second? '()]
          [else
            (append
              (list (list (append first (list (car second))) (cdr second) ))
              (combinate (list first (cdr second))))]
          ))))

(define (combination items)
;  (display ">") (display items) (newline)
  (if (null? items)
      '()
    (append items (combination (flatmap combinate items)))))

(define (main items)
  (map car (combination (list (list '() items)))))








