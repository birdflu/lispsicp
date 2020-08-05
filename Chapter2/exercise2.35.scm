#lang scheme
(include "2.2.2.1.rkt")
(include "2.2.3.1.rkt")

; Redefine count-leaves from Section 2.2.2
; as an accumulation:

(define (new-count-leaves t)
  (accumulate
   (λ (x y) (+ 1 y))
   0
   (map (λ (x) x) (enumerate-tree t))))

;(newline)
;(list x x)
;(count-leaves (list x x))
;(new-count-leaves (list x x))
