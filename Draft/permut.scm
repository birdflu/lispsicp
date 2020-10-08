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

(define (perm first first-iterator second second-iterator)
  (display first-iterator) (display second-iterator) (newline)
  (if (null? first-iterator) 
      '()
      (if (null? second-iterator)
          (if (null? first-iterator)
              '()
              (perm first (cdr first-iterator) second second))
          (append 
            (list (cons (car first-iterator) (list (car second-iterator)))) 
            (perm first first-iterator second (cdr second-iterator))))))
















(perm '(1 2 3) '(1 2 3) '(7 8 9) '(7 8 9))
