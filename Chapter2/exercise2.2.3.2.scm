; Given a positive integer n, find all ordered pairs of distinct positive
; integers i and j, where 1 < j < i < n

(include "../utils.scm")

(define (generate-pairs init n)
  (accumulate
   append nil (map (λ (i)
                     (map (λ (j) (list i j))
                         (enumerate-interval init (- i 1))))
                  (enumerate-interval init n))))

(define (generate-pairs-2 j n)
  (if (= n j)
     nil
     (append
      (map (λ (i) (list i j)) (enumerate-interval (+ 1 j) n))
      (generate-pairs-2 (+ j 1) n)
      )))

(define (generate-pairs-3 init n)
  (flatmap (λ (i)
             (map (λ (j) (list i j))
                 (enumerate-interval init (- i 1))))
          (enumerate-interval init n)))
