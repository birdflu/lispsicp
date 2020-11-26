#lang scheme
(require rackunit)
(include "../exercise2.68.scm")

(check-equal? 
 (encode (decode sample-message sample-tree) sample-tree) sample-message)

