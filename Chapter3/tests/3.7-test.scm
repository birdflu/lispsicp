#lang scheme

(require rackunit)
(include "../exercise3.7.scm")
(define acc (make-account 100 'secret-password))

(define peter-acc (make-account 100 'open-sesame))
(check-equal?
  ((peter-acc 'open-sesame 'withdraw) 1)
  99 
  )

(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
(check-equal?
 ((paul-acc 'rosebud 'withdraw) 1)
  98 
  )

