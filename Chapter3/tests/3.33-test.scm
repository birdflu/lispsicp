#lang scheme

(require rackunit)
(include "../exercise3.33.scm")

(check-equal?
  (procedure? (probe "A" A))
  '#t
  )

(check-equal?
  (procedure? (probe "B" B))
  '#t
  )

(check-equal?
  (procedure? (probe "C" C))
  '#t
  )

(check-equal?
  (averager A B C)
  'ok
  )

(check-equal?
  (set-value! B 10 'user)
  'done
  )

(check-equal?
  (set-value! C 40 'user)
  'done
  )