#lang scheme
(include "../utils.scm")
; () (123) -> 
; (1)(23)
; (2)(13)
; (3)(12)
; 
; (2)(13) ->
; (21)(3)
; (23)(1)
; 
; (23)(1) ->
; (231)()

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

        
;(perm  '() '(1 2 3) '(1 2 3))
;(perm  '(2) '(1 3) '(1 3))
;(permutate  '(( 2 ) ( 1 3 )))
;(permutate  '(( 2 1 ) ( 3 )))
;(perm '(2 1) '(3) '(3))
;(permutation  '(() (1 2)))
;(permutation  '(() (1 2 3)))
;(permutate  '(() (1 2 3)))
;(flatmap permutate (permutate  '(() (1 2 3))))
;(flatmap permutate (flatmap permutate (permutate  '(() (1 2 3)))))
;(flatmap permutate (flatmap permutate (flatmap permutate (permutate  '(() (1 2 3))))))
;(permutation (permutate '(() (1 2 3))))
(permutation '((() (1 2 3))))







