#lang scheme

(require rackunit)
(include "../exercise3.2.scm")

(define s (make-monitored sqrt))

(check-equal?
  (s 100)
  10
  )

(check-equal?
  (s 100)
  10
  )

(check-equal?
  (s 'how-many-calls?)
  2
  )

(check-equal?
  (s 'reset-count)
  0
  )

(check-equal?
  (s 100)
  10
  )

(check-equal?
  (s 'how-many-calls?)
  1
  )


