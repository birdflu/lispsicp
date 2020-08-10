; nil
(define nil '())

; enumerate integers
(define (enumerate-interval low high)
  (if (> low high)
     nil
     (cons low (enumerate-interval (+ low 1) high))))

; enumerate tree leaves
(define (enumerate-tree tree)
  (if (pair? tree)
     (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree)))
     (if (null? tree) tree (list tree))
     ))

; filter
(define (filter predicate sequence)
  (if (null? sequence)
     sequence
     (if (predicate (car sequence))
        (cons (car sequence) (filter predicate (cdr sequence)))
        (filter predicate (cdr sequence)))))

; accumulate
(define (accumulate op init sequence)
  (cond ((null? sequence) init)
       (else (op (car sequence) (accumulate op init (cdr sequence))))))

; flatmap
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; print list
(define (print items)
  (define (print-list items)
    (if (null? items)
       nil
       (if (pair? items)
          (cons (cons 'list (car items)) (print-list (cdr items)))
          (cons 'list items))))
  (cons 'list
       (print-list items)))

; less-then for lists
(define list-less-than?
  (λ (x y) (if (or (null? x) (null? y))
              #f
              (if (= (car x) (car y))
                 (list-less-than? (cdr x) (cdr y))
                 (< (car x) (car y))))))
