#lang scheme

; f (x,y) = x(1 + xy)^2 + y(1 - y) + (1 + xy)(1 - y);
; a = 1 + xy;
; b = 1 - y;
; f (x,y) = xa^2 + yb + ab
; it is written with 4 variants: named procedure f-helper, lambda, let and define 

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

(define (g x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (h x y)
  (let (
        (a (+ 1 (* x y)))
        (b (- 1 y))
        )
    (+ (* x (square a))
       (* y b)
       (* a b))
    )
  )
   
(define (d x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b))
  )
      
   

(define (square x) (* x x))

(f 2 3)
(g 2 3)
(h 2 3)
(d 2 3)
