#lang scheme

(require rackunit)
(include "../exercise3.25.scm")

(define l (list (cons '2 'A) (cons '1 'A) (cons '10 'A)))

(check-equal?
  (put l 'AAA)
  'ok
  )

(check-equal?
  (get (list (cons '2 'A) (cons '1 'A)))
  '{AAA AAA}
  )

(check-equal?
  (put (list (cons '2 'A) (cons '3 'C)) 'CCC)
  'ok
  )

(check-equal?
  (get (list (cons '2 'A) (cons '1 'A)))
  '{CCC AAA}
  )

