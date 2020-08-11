; see 2.41-test.scm

; Write a procedure to find all ordered triples
; of distinct positive integers i, j, and k less than or equal to
; a given integer n that sum to a given integer s.

;(include "../utils.scm")
(include "2.2.3.2.scm")

(define (generate-triples init n)
  (flatmap (λ (k)
             (flatmap (λ (i)
                        (map (λ (j) (list j i k))
                            (enumerate-interval init (- i 1))))
                     (enumerate-interval init (- k 1)))
             )
          (enumerate-interval init n)))

(define (unique-triples-sum-of n s)
  (filter (λ (x) (= (accumulate + 0  x) s)) (generate-triples 1 n)))
  
