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
  (display (list " input :" result source tail))
  (newline)
  (if (null? tail)
      '();result
      (let ((tail (append (cdr tail) (list (car tail))))
            (item (list (car source))))
        (if (null? source)
            ;(append result (permutations result tail tail))
            result
            (let ((permutation (list (append result item) 
                                     source 
                                     (remove-sublist item tail)))
                  )
              (display "result:") (display result)
              (display ", source:") (display source)
              (display ", tail:") (display tail)
              (display ", permutation:") (display permutation)
              (newline)
              (if (null? (cdr source))
                  (cons permutation 
                        (permutations (car permutation) 
                                      (caddr permutation) 
                                      (caddr permutation)));(list permutation)
                  (cons permutation 
                        (permutations result (cdr source) tail))))))))

(define (remove-sublist sublist list)
  (if (null? sublist)
      list
      (remove-sublist (remove (car sublist) sublist) (remove (car sublist) list))))

(permutations '(2) '(1 3) '(1 3))
;(permutations '() '(1 2 3) '(1 2 3))
;(permutations '(2 3) '(1) '(1))
;(permutations '(2 3 1) '() '())
