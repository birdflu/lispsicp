#lang scheme

(require rackunit)
(include "../exercise3.12.scm")

(check-equal?
  z
  '(a b c d)
  )

(check-equal?
  (cdr x)
  '(b)
  )

(define w (append! x y))

(check-equal?
  w
  '(a b c d)
  )

(check-equal?
  (cdr x)
  '(b c d)
  )
