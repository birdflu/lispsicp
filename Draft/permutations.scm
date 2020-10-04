#lang scheme

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

(define (permutations result source)
  (display "result:") (display result)
  (display ", source:") (display source)
  (newline)
  (if (null? source)
      result
      (let ((permutation (append result (list (car source)))))
        (if (null? (cdr source))
          (list permutation)
          (cons permutation 
              (permutations result (cdr source)))))))

(permutations '(8) '())
(permutations '() '(1 2 3 4))
(permutations '(8) '(1 2 3 4))
(permutations '(2) '(1 3 4))
(permutations '(2 3 4)  '(1))
 
(define (expand items len)
  (display "items=") (display items)
  (newline)
  (if (< len 0)
      '()
      (let ((source (cadr items))
            (result (car items)))
        (cons 
          (append result (list (car source)))
          (expand (list (append result (list (car source))) 
                       (append (cdr source) (list (car source)))) 
                      ;(cdr source))
                  (- len 1))))))

;(expand '(() (1 2 3)) 3)
;(expand '((1) (2 3)) 2)
