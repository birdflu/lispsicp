(include "2.2.3.1.rkt")
(include "2.36.rkt")

(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define v (list 1 1 5))
(define w (list 2 2 10))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (λ (x) (accumulate + 0 x)) m))
