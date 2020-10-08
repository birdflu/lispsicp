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

(define (perm first second constant tail)
;  (display first) (display second) (newline)
  (let ((null-first? (null? first))
        (null-second? (null? second)))
  (cond [(and null-first? null-second?) '()]
        [(and null-second? (not null-first?))
         (perm  (cdr first) constant constant tail)]
        [(and (not null-second?) null-first?)
         '()]
;          (cons (list (car second)) (perm first (cdr second) constant)) ]
        [else 
          (append 
            (list (cons (car first) (list (car second)))) 
            (perm first (cdr second) constant tail))]
        )))


(define (remove-sublist sublist list)
  (if (null? sublist)
      list
      (remove-sublist (remove (car sublist) sublist) (remove (car sublist) list))))


(define (permutate result tail)
  (map (lambda (x) (cons x (list (remove-sublist x tail))))
       (perm result tail tail '())))






;(perm  '(()) '(1 2 3) '(1 2 3))
;(perm  '(1) '(2 3) '(2 3))
(permutate  '(2) '(1 3))
;(perm  '(3) '(2 1) '(2 1))



