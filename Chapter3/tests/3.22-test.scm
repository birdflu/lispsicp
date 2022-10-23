#lang scheme

(require rackunit)
(include "../exercise3.22.scm")

(check-equal?
  (insert-queue! q1 'b)
  '((b) b)
  )
(check-equal?
  (insert-queue! q1 'c)
  '((b c) c)
  )
(check-equal?
  (front-queue q1)
  '(b c)
  )
(check-equal?
  (front-queue q1)
  '(b c)
  )
(check-equal?
  (delete-queue! q1)
  '((c) c)
  )
(check-equal?
  (delete-queue! q1)
  '(() c)
  )
(check-equal?
  (empty-queue? q1)
  #t
  )


