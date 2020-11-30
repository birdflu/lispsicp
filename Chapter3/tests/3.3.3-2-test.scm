#lang scheme
(require rackunit)

(include "../exercise3.3.3-2.scm")

(check-equal? (put 'a 'b 'ab) 'ok)
(check-equal? (put 'a 'c 'ac) 'ok)

(check-equal? (get 'a 'c) 'ac)
(check-equal? (get 'a 'b) 'ab)

