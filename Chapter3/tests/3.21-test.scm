#lang scheme

(require rackunit)
(include "../exercise3.21.scm")

(check-equal?
  (insert-queue! q1 'a)
  '((a) a)
  )

(check-equal?
  (insert-queue! q1 'b)
  '((a b) b)
  )

(check-equal?
  (print-queue q1)
  '(a b)
  )


