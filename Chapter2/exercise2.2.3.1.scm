#lang scheme

; It takes a tree as argument and computes the sum of the
; squares of the leaves that are odd
(define (sum-odd-squares-old tree)
  (cond
    ((null? tree) 0)
    ((pair? tree) (+ (sum-odd-squares-old (car tree))
                    (sum-odd-squares-old (cdr tree))))
    (else (if (odd? tree) (square tree) 0))))

; It constructs a list of all the even Fibonacci numbers Fib(k), 
; where k is less than or equal to a given integer n:
(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
     b
     (fib-iter (+ a b) a (- count 1))))

(define (even-fibs-old n)
  (define (next k)
    (if (> k n)
       '()
       (let ((f (fib k)))
         (if (even? f)
            (cons f (next (+ k 1)))
            (next (+ k 1))))))
  (next 0))

; enumerate -> filter -> map -> accumulate

; enumerate tree leaves
(define (enumerate-tree tree)
  (if (pair? tree)
     (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree)))
     (if (null? tree) tree (list tree))
     ))

; enumerate integers
(define (enumerate-interval low high)
  (if (= low high)
     (list high)
     (cons low (enumerate-interval (+ 1 low) high))))

; filter odd? and even?
(define (filter predicate sequence)
  (if (null? sequence)
     sequence
     (if (predicate (car sequence))
        (cons (car sequence) (filter predicate (cdr sequence)))
        (filter predicate (cdr sequence)))))

; accumulate [op = +, init = 0] or [op = cons, init =  ()]
(define (accumulate op init sequence)
  (cond ((null? sequence) init)
       (else (op (car sequence) (accumulate op init (cdr sequence))))))

;
(define (sum-odd-squares tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons '() (filter even? (map fib (enumerate-interval 0 n))))
)

(define (square x)
  (* x x))

(define test-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

test-tree
(sum-odd-squares test-tree)

(enumerate-tree test-tree)
(enumerate-interval 1 5)
(filter odd? (enumerate-interval 1 5))
(filter even? (enumerate-interval 1 5))

(cdr (enumerate-interval 1 5))
(accumulate + 0 (enumerate-interval 1 5))
(accumulate * 1 (enumerate-interval 1 5))
(accumulate cons '() (enumerate-interval 1 5))

(sum-odd-squares-old test-tree)
(sum-odd-squares test-tree)
(even-fibs-old 10)
(even-fibs 10)
