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

(define permutate (lambda (items)
  (if (null? items) 
      '()
      (let ((result (car items))
            (source (cadr items))
            (tail (caddr items)))
        (display "=> ") (display result) (display source) (display tail) (newline)
        (let ((item (if (null? source)
                        '()
                        (list (append result (list (car source))) (cdr source) tail))))
          (append (list item) (permutate item)))
      ))))

(define (get-init items len)
  (if (= 0 len) 
      '() 
      (append (list items)
              (get-init (append (cdr items) (list (car items))) (- len 1)))))

(define (add-prefix prefix)
  (lambda (x) (cons prefix (list x))))

;(get-init '(1 2 3) (length '(1 2 3)))
;(map (add-prefix '()) (get-init '(1 2 3) (length '(1 2 3))))
;(flatmap (lambda (x) (permutate x)) (map (add-prefix '()) (get-init '(1 2 3) (length '(1 2 3)))))
;(map car 
;     (filter (lambda (x) (not (null? x)))
;     (flatmap (lambda (x) (permutate x)) (map (add-prefix '()) (get-init '(1 2 3) (length '(1 2 3)))))))
(flatmap (lambda (x) (permutate x)) '( (() (1 2 3) (1 2 3)) ))
