#lang scheme

(require rackunit)
(include "../exercise3.21.scm")

(insert-queue! q1 'a)
(insert-queue! q1 'b)

(check-equal?
  (print-queue q1)
  '(a b)
  )


