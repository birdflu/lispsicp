;#lang scheme
(include "../images.scm")
(include "../utils.scm")

(define xcor-vect car)
(define ycor-vect cdr)

(define (add-list  x1 x2)
  (cons (+ (car x1) (car x2)) (+ (cdr x1) (cdr x2))))

(define (sub-list  x1 x2)
  (cons (- (car x1) (car x2)) (- (cdr x1) (cdr x2))))

(define (scale-vect  c v)
  (make-vect (* c (xcor-vect v)) (* c (ycor-vect v))))

(define (add-vect x . l)
    (add-list x (accumulate add-list (cons 0 0) l)))    

(define (sub-vect x . l)
    (sub-list x (accumulate sub-list (cons 0 0) l)))    
