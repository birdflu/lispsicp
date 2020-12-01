#lang scheme
(require rackunit)
(include "../exercise2.74.scm")

(set-employer-data)

(check-equal? (get 'companies 'companies) '("Small Inc" "Instantiable Inc"))
(check-equal? (get-record  "Master"  "Instantiable Inc")
              (cons
                (cons 'Department "Sale")
                (cons (cons 'Address "Piter") (cons (cons 'Salary 1300) '()))))

(check-equal? (find-employee-record "Master")
              (cons
                (cons
                  (cons 'Department "Sale")
                  (cons (cons 'Address "Piter") (cons (cons 'Salary 1300) '())))
                (cons
                  (cons
                    (cons 'Department "Sale")
                    (cons (cons 'Address "Piter") (cons (cons 'Salary 1300) '())))
                  '())))

