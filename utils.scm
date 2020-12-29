; test git

; nil
(define nil '())

; enumerate integers
(define (enumerate-interval low high)
  (if (> low high)
     nil
     (cons low (enumerate-interval (+ low 1) high))))

; enumerate tree leaves
(define (enumerate-tree tree)
  (if (pair? tree)
     (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree)))
     (if (null? tree) tree (list tree))
     ))

; filter
(define (filter predicate sequence)
  (if (null? sequence)
     sequence
     (if (predicate (car sequence))
        (cons (car sequence) (filter predicate (cdr sequence)))
        (filter predicate (cdr sequence)))))

; accumulate
(define (accumulate op init sequence)
  (cond ((null? sequence) init)
       (else (op (car sequence) (accumulate op init (cdr sequence))))))

; flatmap
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; print list
(define (print items)
  (define (print-list items)
    (if (null? items)
       nil
       (if (pair? items)
          (cons (cons 'list (car items)) (print-list (cdr items)))
          (cons 'list items))))
  (cons 'list
       (print-list items)))

; less-then for lists
(define list-less-than?
  (λ (x y) (if (or (null? x) (null? y))
              #f
              (if (= (car x) (car y))
                 (list-less-than? (cdr x) (cdr y))
                 (< (car x) (car y))))))

; quick-sort
(define (quick-sort rule items)
  ; 1. Выбрать первый элемент из массива. Назовём его опорным.
  ; 2. Разбиение: перераспределение элементов в массиве таким образом,
  ;    что элементы меньше опорного помещаются перед ним, а больше или равные после.
  ; 3. Рекурсивно применить первые два шага к двум подмассивам слева и справа
  ;    от опорного элемента. Рекурсия не применяется к массиву, в котором
  ;    только один элемент или отсутствуют элементы.
    (define (divide left right pivot items)
      (display items) (newline)
      (display "[") (display left) (display right) (display "]") (newline)
      (cond ((and (eq? items '()) (eq? left '()) (eq? right '())) '())
            ((and (eq? items '()) (eq? left '())) (divide '() '() (car right) right))
            ((and (eq? items '()) (eq? right '())) (divide '() '() (car left) left))
            ((and (not (eq? items '())) (eq? (cdr items) '()) (eq? left '()) (eq? right '()))
             items)
            ((eq? items '())
             (append (divide '() '() (car left) left) (divide '() '() (car right) right)))
            (else (if (rule (car items) pivot)
                      (divide (cons (car items) left) right pivot (cdr items))
                      (divide left (cons (car items) right) pivot (cdr items))))))
    (if (eq? items '())
        items
        (divide '() '() (car items) items)))
