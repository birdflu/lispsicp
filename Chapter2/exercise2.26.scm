#lang scheme

;What result is printed by the interpreter in response to evaluating
;each of the following expressions:

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)   ; (1 2 3 4 5 6)
(cons x y)     ; ((1 2 3) 4 5 6)
(list x y)     ; ((1 2 3) (4 5 6))
