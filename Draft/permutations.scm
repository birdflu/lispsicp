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

(define (permutations result source tail)
; (let ((tail (remove (car source)  source)))
  (let ((tail (append (cdr source) (list (car source)))))
  (display "result:") (display result)
  (display ", source:") (display source)
  (display ", tail:") (display tail)
  (newline)
     (if (null? source)
      result
      (let ((permutation (list (append result (list (car source))) tail)))
        (if (null? (cdr source))
          (list permutation)
          (cons permutation 
              (permutations result (cdr source) (append tail (car source)))))))))

(define (remove-sublist sublist list)
  (if (null? sublist)
      list
      (remove-sublist (remove (car sublist) sublist) (remove (car sublist) list))))

(permutations '(2) '(1 3) '())
(permutations '() '(1 2 3) '())


