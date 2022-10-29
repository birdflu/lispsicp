#lang scheme

(require rackunit)
(include "../exercise3.23.scm")

(define d1 (make-deque))

(check-equal?
  (empty-deque? d1)
  #t
  )

(check-equal?
  (begin
    (rear-insert-queue! d1 'b)
    (print-deque d1))
  '{b}
  )

(check-equal?
  (begin
    (rear-insert-queue! d1 'c)
    (print-deque d1))
  '{b c}
  )

(check-equal?
  (begin
    (front-insert-queue! d1 'a)
    (print-deque d1))
  '{a b c}
  )

(check-equal?
  (empty-deque? d1)
  #f
  )

(check-equal?
  (begin
    (rear-delete-queue! d1)
    (print-deque d1))
  '{a b}
  )

(check-equal?
  (begin
    (front-delete-queue! d1)
    (print-deque d1))
  '{b}
  )

(check-equal?
  (begin
    (rear-delete-queue! d1)
    (print-deque d1))
  '{}
  )

(check-equal?
  (empty-deque? d1)
  #t
  )

