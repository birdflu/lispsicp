(include "../Chapter3/exercise3.3.3-2.scm")
(include "exercise2.3.3.scm") 

(define (set-employer-data)
  (define (put-employer employer company department address salary)
    (put 'company (cons company employer) company)
    (put 'department (cons company employer) department)
    (put 'address (cons company employer) address)
    (put 'salary (cons company employer) salary)
   
    (if (not (get 'companies 'companies))
        (put 'companies 'companies (cons company '())))

    (if (not (element-of-set? company (get 'companies 'companies)))
        (put 'companies 'companies (cons company (get 'companies 'companies)))))
  
  (put-employer "Mark" "Instantiable Inc" "Sale" "Moscow" 300)
  (put-employer "Aliona" "Instantiable Inc" "Sale" "Paris" 1300)
  (put-employer "Piter" "Instantiable Inc" "Sale" "Moscow" 2300)
  (put-employer "Linda" "Instantiable Inc" "Sale" "Kiev" 3300)
  (put-employer "Master" "Instantiable Inc" "Sale" "Piter" 1300)
  (put-employer "Anna" "Small Inc" "Sale" "Piter" 1300)
  (put-employer "Master" "Small Inc" "Sale" "Piter" 1300)
  )

(define (get-record employer company)
  (list
    (cons 'Department (get 'department (cons company employer)))
    (cons 'Address (get 'address (cons company employer)))
    (cons 'Salary (get 'salary (cons company employer)))))

(define (get-salary employer company)
  (get 'salary (cons company employer)))

(define (find-employee-record employer)
  (map (λ (x) (get-record employer x)) (get 'companies 'companies)))

