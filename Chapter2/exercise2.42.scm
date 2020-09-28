; see 2.42-test.scm

; The “eight-queens puzzle”

(include "../utils.scm")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
       (list empty-board)
       (filter
        (λ (positions) (safe? k positions))
        (flatmap
         (λ (rest-of-queens)
           (map (λ (new-row)
                  (adjoin-position
                   new-row k rest-of-queens))
               (enumerate-interval 1 board-size)))
         (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board nil)

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (list new-row k))))
 
(define row-column-neighborhood?
  (λ (a b)
    (and
     (not (equal? a nil))
     (or (= (car a) (car b)) (= (cadr a) (cadr b)))
     ;(not (and (= (car a) (car b)) (= (cadr a) (cadr b)))))))
     (not (equal? a b)))))

(define (diagonal-neighborhoods item)
  (define (neighborhoods item min max direction)
    (let ((x (car item))
          (y (cadr item))
          (x-operation (if (or (equal? direction "SE") (equal? direction "SW")) + -))
          (y-operation (if (or (equal? direction "SE") (equal? direction "NE")) + -)))
      (if (and (>= x min) (<= x max) (>= y min) (<= y max)) 
         (cons item (neighborhoods (list (x-operation x 1) (y-operation y 1)) min max direction))
         '())))
  (append
   (remove item (neighborhoods item 1 8 "NE"))
   (remove item (neighborhoods item 1 8 "NW"))
   (remove item (neighborhoods item 1 8 "SE"))
   (remove item (neighborhoods item 1 8 "SW"))))

(define (safe? k positions)
  (let ((item (car (filter (λ (x) (= (cadr x) k)) positions))))
    (equal? (filter (λ (x) (row-column-neighborhood?  item x)) positions)
           (filter (λ (x) (diagonal-neighborhoods?  item x)) positions))))
  
(define diagonal-neighborhoods?
  (λ (a b)
    (not (equal? nil
                (filter (λ (x) (equal? a x))
                       (diagonal-neighborhoods b))))))
