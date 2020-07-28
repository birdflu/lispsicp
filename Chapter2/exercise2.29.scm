#lang scheme
; A binary mobile consists of two branches,
; a left branch and a right branch. Each branch is a rod of
; a certain length, from which hangs either a weight or another
; binary mobile.

; a. Write the corresponding selectors left-branch and
; right-branch, which return the branches of a mobile,
; and branch-length and branch-structure, which return
; the components of a branch.

; b. Using your selectors, define a procedure total-weight
; that returns the total weight of a mobile.

; c. A mobile is said to be balanced if the torque applied by
; its top-left branch is equal to that applied by its top-right
; branch (that is, if the length of the left rod multiplied
; by the weight hanging from that rod is equal
; to the corresponding product for the right side) and if
; each of the submobiles hanging off its branches is balanced.
; Design a predicate that tests whether a binary
; mobile is balanced.

; d. Suppose we change the representation of mobiles so
; that the constructors are
;    (define (make-mobile left right)
;      (cons left right))
; instead
;    (define (make-mobile left right)
;      (list left right))
; and
;    (define (make-branch length structure)
;      (cons length structure))
; instead
;    (define (make-branch length structure)
;      (list length structure))
; How much do you need to change your programs to
; convert to the new representation?

; ====vesion 1 (without clause d) ========

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

; ====end vesion 1 (without clause d) ========

; ====vesion 2 (with clause d) ========

;(define (make-mobile left right)
;  (cons left right))
;
;(define (make-branch length structure)
;  (cons length structure))
;
;; is item cons construction?
;(define (cons? item)
;  (or 
;   (and (pair? (car item)) (pair? (cdr item)) (not (pair? (car (car item)))) (not (pair? (car (cdr item))))) 
;   (and (not (pair? (car item))) (not (pair? (cdr item))))))
;
;(define (cons-to-list item)
;  (if (null? item)
;     item
;     (if (pair? item) 
;        (if (cons? item)
;           (list (cons-to-list (car item)) (cons-to-list (cdr item)))
;           item) 
;        item)))
;
;(define (left-branch mobile)
;  (display "mobile for left=")(display mobile)(newline)
;  (display "cons-to-list=")(display (cons-to-list mobile))(newline)
;  (display "left-branch=")(display (car (cons-to-list mobile)))(newline)
;  (car (cons-to-list mobile)))
;
;(define (right-branch mobile)
;  (display "mobile for right=")(display mobile)(newline)
;  (display "cons-to-list=")(display (cons-to-list mobile))(newline)
;  (display "right-branch=")(display (car (cdr (cons-to-list mobile))))(newline)
;  (car (cdr (cons-to-list mobile))))

; ====end vesion 2 (with clause d) ========

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch))
  )

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
(left-branch b)
(branch-length (left-branch b))
(right-branch b)
(branch-length (right-branch b))
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
