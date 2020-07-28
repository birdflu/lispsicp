#lang scheme

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define c
  (make-mobile
   (make-branch 5 1)
   (make-branch 1 5)))

(define d
  (make-mobile
   (make-branch 3 2)
   (make-branch 3 5)))

(define b
  (make-mobile
   (make-branch 2 c)
   (make-branch 2 d)))

(define a
  (make-mobile
   (make-branch 9 8)
   (make-branch 10 b)))

(define balance-mobile
  (make-mobile
   (make-branch 2 a)
   (make-branch 2 a)))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-length mobile)
  (let ((left-br (left-branch mobile))
        (right-br (right-branch mobile)))
    (if (null? mobile)
       0
       (+ (branch-length left-br)
         (branch-length right-br)
         (if (pair?  (branch-structure left-br))
            (total-length (branch-structure left-br))
            0)
         (if (pair?  (branch-structure right-br))
            (total-length (branch-structure right-br))
            0)))))

(define (total-weight mobile)
  (let ((left-br (left-branch mobile))
        (right-br (right-branch mobile)))
    (if (null? mobile)
       0
       (+ (if (pair?  (branch-structure left-br))
             (total-weight (branch-structure left-br))
             (branch-structure left-br))
         (if (pair?  (branch-structure right-br))
            (total-weight (branch-structure right-br))
            (branch-structure right-br))))))

(define (rotational-moment mobile)
  (let ((left-br (left-branch mobile))
        (right-br (right-branch mobile)))
    (if (null? mobile)
       0
       (+ (* -1
            (if (pair? (branch-structure left-br))
               (* (branch-length left-br) (rotational-moment (branch-structure left-br)))
               (* (branch-structure left-br) (branch-length left-br))))
         (if (pair?  (branch-structure right-br))
            (* (branch-length right-br) (rotational-moment (branch-structure right-br)))
            (* (branch-structure right-br) (branch-length right-br)))))))

(define (balance? mobile)
  (= 0 (rotational-moment mobile)))

(display "c=") c
(display "total-length=") (total-length c)
(display "total-weight=") (total-weight c)
(display "rotational-moment=") (rotational-moment c)
(newline)
(display "b=") b
(display "              /\\") (newline)
(display "          2/    \\2") (newline)
(display "        5/\\1 3/\\3") (newline)
(display "         1 5  2 5") (newline)
(display "total-length=") (total-length b)
(display "total-weight=") (total-weight b)
(display "rotational-moment=") (rotational-moment b)
(newline)
(display "a=") a
(display "total-length=") (total-length a)
(display "total-weight=") (total-weight a)
(display "rotational-moment=") (rotational-moment a)
(display "balance?=") (balance? a)

(display "balance-mobile=") balance-mobile
(display "total-length=") (total-length balance-mobile)
(display "total-weight=") (total-weight balance-mobile)
(display "rotational-moment=") (rotational-moment balance-mobile)
(display "balance?=") (balance? balance-mobile)
