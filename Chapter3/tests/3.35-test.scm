#lang scheme

(require rackunit)
(include "../exercise3.35.scm")

(check-equal?
  (procedure? (probe "A" A))
  '#t
  )

(check-equal?
  (procedure? (probe "B" B))
  '#t
  )


(check-equal?
  (calcsquarer A B)
  'ok
  )

(check-equal?
  (set-value! B 9 'user)
  'done
  )
