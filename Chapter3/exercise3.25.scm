(require rnrs)
(require rnrs/mutable-pairs-6)

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-pair)
      (let ((subtable
             (assoc (car key-pair) (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc (cdr key-pair) (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (lookup-list key-list)
      (map lookup key-list))
    (define (insert-key-pair-value! key-pair-value)
      (define (insert! key-pair value)
        (let ((subtable
               (assoc (car key-pair) (cdr local-table))))
          (if subtable
              (let ((record
                     (assoc (cdr key-pair) (cdr subtable))))
                (if record
                    (set-cdr! record value)
                    (set-cdr! subtable
                              (cons (cons (cdr key-pair) value)
                                    (cdr subtable)))))
              (set-cdr! local-table
                        (cons (list (car key-pair) (cons (cdr key-pair) value))
                              (cdr local-table))))))
      (insert! (car key-pair-value) (cdr key-pair-value)))
    (define (insert-list! key-list value)
      (map insert-key-pair-value! (map (λ(x) (cons x value)) key-list))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup-list)
            ((eq? m 'insert-proc!) insert-list!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

