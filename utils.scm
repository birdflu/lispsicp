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

; distinct
(define (distinct sorted-items)
  (if (empty? sorted-items)
    '()
    (if (empty? (cdr sorted-items))
      sorted-items
      (if (equal? (car sorted-items) (cadr sorted-items))
        (distinct (cons (car sorted-items) (cddr sorted-items)))
        (cons (car sorted-items) (distinct (cdr sorted-items)))))))

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
;
; (quick-sort < (λ(x) x) '(1 1 6 4 6 1 4 -1))
; (-1 1 1 1 4 4 6 6)
; (quick-sort < car '((6 1) (6 3) (1 3) (3 3) (3 4) (1 1) (6 0)))
; ((1 1) (1 3) (3 3) (3 4) (6 1) (6 3) (6 0))
(define (quick-sort rule comparison-rule items)
  ; 1. Выбрать первый элемент из массива. Назовём его опорным.
  ; 2. Разбиение: перераспределение элементов в массиве таким образом,
  ;    что элементы меньше опорного помещаются перед ним, а больше или равные после.
  ; 3. Рекурсивно применить первые два шага к двум подмассивам слева и справа
  ;    от опорного элемента. Рекурсия не применяется к массиву, в котором
  ;    только один элемент или отсутствуют элементы.
  (define (candidate items)
    (if (empty? items)
        '()
        (car items)))
  (define (comparison-base pivot-candidate)
    (if (empty? pivot-candidate)
        '()
        (comparison-rule pivot-candidate)))
  (define (divide left right pivot items)
;    (display items) (newline)
;    (display "[") (display left) (display pivot) (display right) (display "]") (newline)
    (define (tail items)
      (if (eq? items '())
          '()
          (cdr items)))
    (cond ((empty? pivot)
           '())
          ((and (not (empty? pivot)) (empty? items) (empty? left) (empty? right))
           (list pivot))
          ((and (not (empty? pivot)) (not (empty? items)) (empty? (tail items))
                (empty? left) (empty? right))
           (if (rule (comparison-base pivot) (comparison-base (candidate items)))
               (cons pivot items)
               (cons (candidate items) (list pivot))))
          ((and (not (empty? pivot)) (empty? items))
           (append
             (divide '() '() (candidate left) (tail left))
             (list pivot)
             (divide '() '() (candidate right) (tail right))))
          ((and (not (empty? pivot)) (not (empty? items)))
           (if (rule (comparison-base (candidate items)) (comparison-base pivot))
               (divide (cons (candidate items) left) right pivot (tail items))
               (divide left (cons (candidate items) right) pivot (tail items))))))
  (if (empty? items)
      items
      (divide '() '() (candidate items) (cdr items))))

;(sparse->sparse-with-zeros '{{6 9} {5 6} {5 6} {4 3} {4 4} {4 3} {3 15} {3 15} {3 2} {3 2} {2 10} {2 10} {2 1} {1 5} {{1 5}} {0 25}})

;(quick-sort > car '{{1 5} {0 25} {1 5} {3 15}} )
;{{3 15} {1 5} {{1 5}} {0 25}}
;(quick-sort > (λ (x) x) '{1 0 1 3} )
;{3 1 {1} 0}

