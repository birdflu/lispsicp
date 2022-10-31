#lang scheme

(require rackunit)
(include "../exercise3.37.scm")

(check-equal?
  (procedure? (probe "Celsius temp" C))
  '#t
  )

(check-equal?
  (procedure? (probe "Fahrenheit temp" F))
  '#t
  )

(check-equal?
  (set-value! C 25 'user) 
  'done
  )

  