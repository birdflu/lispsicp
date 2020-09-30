#lang scheme
(include "exercise2.3.3.3.scm")
(define (tree->list-1 tree)
  (display ".")
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (display ".")
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
  

(define a '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))) 
(define b '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define c '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
(define d '(1 () (2 () (3 () (4 () (5 () (6 () (7 () ()))))))))

(define (test trees)
 (if (null? trees) 
      "end test" 
      (begin 
        (display (tree->list-1 (car trees))) (newline)
        (display (tree->list-2 (car trees))) (newline)
        (test (cdr trees)))))

(test (list a b c d))
