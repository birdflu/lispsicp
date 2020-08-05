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

(define (new2-count-leaves t)
    (accumulate +
                0
                (map (λ (sub-tree)
                         (if (pair? sub-tree)
                             (new2-count-leaves sub-tree)
                             1))
                     t)))

;(newline)
;(list x x)
;(count-leaves (list x x))
;(new-count-leaves (list x x))
;(new2-count-leaves (list x x))
