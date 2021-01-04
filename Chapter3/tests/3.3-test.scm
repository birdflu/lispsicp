#lang scheme

(require rackunit)
(include "../exercise3.3.scm")
(define acc (make-account 100 'secret-password))

(check-equal?
  ((acc 'secret-password 'withdraw) 40)
  60
  )

(check-equal?
  ((acc 'some-other-password 'deposit) 50)
  "Incorrect password"
  )


