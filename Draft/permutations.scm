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
;  (display (list " input :" result source tail)) (newline)
;  (if (and (null? tail) (null? source))
  (if (null? tail)
       (list result source tail)
  (let ((tail (append (cdr tail) (list (car tail))))
        (item (list (car source))))
    ;    (display "item") (display item) (newline)
    (let ((permutation (list (append result item) 
                             source 
                             (remove-sublist item tail))))
      ;              (display "result:") (display result)
      ;              (display ", source:") (display source)
      ;              (display ", tail:") (display tail)
      (display ", permutation:") (display permutation)
      (newline)
      (if (null? (cdr source))
          (begin ;(display "NULL cdr source") (newline) 
            (if (pair? (caar permutation))
                (begin 
                  (display "PAIR PERM: ") (display permutation) (newline)
                  permutation)
                (begin 
                  ;                             (display "NOT PAIR PERM: ") (display permutation) (newline)
                  permutation)))
          (begin
            ;                    (display "RRRR-PERM-CONS") (display permutation) (newline)
            ;                    (display "PERM-PERM") (display (permutations result (cdr source) tail)) (newline)
            ;                    (display "PERM-RESULT") (display result) (newline)
            ;                    (display "PERM-cdr-SOURCE") (display (cdr source)) (newline)
            ;                    (display "PERM-TAIL") (display tail) (newline)
            (let ((p (permutations result (cdr source) tail)))
              (let ((r (append (if (pair? (caar permutation)) (flat permutation) (list permutation))
                               (if (pair? (caar p)) (flat p) (list p)))))
                ;(let ((r (append 
                ;          (flat permutation) 
                ;          (flat p))))
                (display "PERM: ") (display r) (newline)
                (map (lambda (x) 
                       (permutations (car x) (caddr x) (caddr x))) 
                     r)
                )))
          )))));)

(define (flat items)
  ;  (display items)
  ;  (newline)
  (if (null? items)
      items
      (let ((head (car items))
            (tail (cdr items)))
        ;        (display "head: ") (display head)
        ;        (display ", tail: ") (display tail)
        ;        (newline)
        (if (pair? (caar head))
            (cons (car head) (cons (cadr head) (flat tail)))
            (cons head (flat tail))))))

(define z '(((1) (1 2 3) (2 3)) (((2 3 1) (1) ()) ((2 1 3) (3) ())) (((3 1 2) (2) ()) ((3 2 1) (1) ()))))
;(((1) (1 2 3) (2 3)) ((2 3 1) (1) ()) ((2 1 3) (3) ()) ((3 1 2) (2) ()) ((3 2 1) (1) ()))
;z
;(flat z)
;(flat (flat z))

(define (remove-sublist sublist list)
  (if (null? sublist)
      list
      (remove-sublist (remove (car sublist) sublist) (remove (car sublist) list))))

;((((2 3 1) (1) ())) (((2 1 3) (3) ())))

;(((1) (1 2 3) (2 3))
; ((((2 3 1) (1) ())) (((2 1 3) (3) ())))
; ((((3 1 2) (2) ())) (((3 2 1) (1) ()))))

;(permutations '(2) '(1 3) '(1 3))
;(permutations '() '(1 2 3 4) '(1 2 3 4))
(permutations '() '(1 2) '(1 2))
(permutations '() '(1 2 3) '(1 2 3))
;(permutations '(1 3) '(2) '(2))

;(permutations '(2 3) '(1) '(1))
;(permutations '(2 3 1) '() '())
