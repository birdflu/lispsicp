#lang scheme

(define (halfInterval f left right)
  (let ((avg (/ (+  left right) 2))
        )
    (define next (f avg))
    (display (list "leftArg =" left " rightArg =" right "f(avg ="avg ")=" next))
    (newline)
    
    (if (close? left right) avg
        (if (< 0 next)
            (halfInterval f left avg)
            (halfInterval f avg right )
            )
        )
    )
  )
(define tolerance 0.0001)

(define (close? x y) (< (abs (- x y)) tolerance))

(halfInterval sin 6 6.5)
