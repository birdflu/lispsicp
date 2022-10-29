(require rnrs)
(require rnrs/mutable-pairs-6)

(define empty-triple
  (cons (cons '() '()) '()))

(define (new-triple value)
  (define (triple value previous-triple next-triple)
    (cons (cons value previous-triple) next-triple))
  (triple value empty-triple empty-triple))

(define (get-next triple)
  (cdr triple))

(define (get-prev triple)
  (cdr (car triple)))

(define (update-next triple new-element)
  (set-cdr! triple new-element))

(define (update-prev triple new-element)
  (set-car! triple (cons (car (car triple)) new-element)))

(define (update-value triple value)
  (set! triple (cons (cons value (get-prev triple)) (get-next triple)))
  )

(define (get-value triple)
  (car (car triple)))

(define (insert-next triple value)
  (update-next triple value)
  )

(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (insert-into-empty-deque new-element)
      (set! front-ptr new-element)
      (set! rear-ptr new-element))
    
    (define (front-insert-deque! item)
      (let ((new-element (new-triple item)))
        (cond ((null? front-ptr)
               (insert-into-empty-deque new-element))
              (else
               (update-next new-element front-ptr)
               (update-prev front-ptr new-element)
               (set! front-ptr new-element)))))

    (define (rear-insert-deque! item)
      (let ((new-element (new-triple item)))
        (cond ((null? front-ptr)
               (insert-into-empty-deque new-element))
              (else
               (update-prev new-element rear-ptr)
               (update-next rear-ptr new-element)
               (set! rear-ptr new-element)))))
    
    (define (rear-delete-queue!)
      (cond ((null? front-ptr)
             (error "DELETE! called with an empty deque" (cons front-ptr rear-ptr)))
            ((eq? front-ptr rear-ptr)
             (set! front-ptr '())
             (set! rear-ptr '()))
            (else (set! rear-ptr (get-prev rear-ptr))
                  (update-next rear-ptr '()))))

    (define (front-delete-queue!)
      (cond ((null? front-ptr)
             (error "DELETE! called with an empty deque" (cons front-ptr rear-ptr)))
            ((eq? front-ptr rear-ptr)
             (set! front-ptr '())
             (set! rear-ptr '()))
            (else (set! front-ptr (get-next front-ptr))
                  (update-prev front-ptr '()))))
    
    (define (front-deque)
      (if (null? front-ptr)
          (error "FRONT called with an empty deque" (cons front-ptr rear-ptr))
          front-ptr))

    (define (rear-deque)
      (if (null? rear-ptr)
          (error "REAR called with an empty deque" (cons front-ptr rear-ptr))
          rear-ptr))
    
    (define (dispatch m)
      (cond ((eq? m 'empty?)
             (null? front-ptr))
            ((eq? m 'front-insert)
             front-insert-deque!)
            ((eq? m 'rear-insert)
             rear-insert-deque!)
            ((eq? m 'front-delete)
             front-delete-queue!)
            ((eq? m 'rear-delete)
             rear-delete-queue!)
            ((eq? m 'front)
             front-deque)
            ((eq? m 'rear)
             rear-deque)
            (else (error "Unknown operation" m))))
    dispatch))

(define (empty-deque? deque)
  (deque 'empty?))

(define (front-insert-queue! deque item)
  ((deque 'front-insert) item))

(define (rear-insert-queue! deque item)
  ((deque 'rear-insert) item))

(define (front-delete-queue! deque)
  ((deque 'front-delete)))

(define (rear-delete-queue! deque)
  ((deque 'rear-delete)))

(define (front-deque deque)
  ((deque 'front)))

(define (rear-deque deque)
  ((deque 'rear)))

(define (print-deque deque)
  (define (collect-element rear-ptr collection)
    (if (eq? rear-ptr '())
        collection
        (collect-element (get-prev rear-ptr) (cons (get-value rear-ptr) collection))))
  
  (cond ((empty-deque? deque) '())
        ((eq? (rear-deque deque) (front-deque deque))
         (list (car (car (rear-deque deque)))))
        (else (cdr (collect-element (rear-deque deque) '())))))

