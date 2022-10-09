(require rnrs)
(require rnrs/mutable-pairs-6)


(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)


(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

;z
; (mcons 'a (mcons 'b (mcons 'c (mcons 'd '()))))

;(cdr x)
; (mcons 'b '())

;(define w (append! x y))

;w
; (mcons 'a (mcons 'b (mcons 'c (mcons 'd '()))))

;(cdr x)
; (mcons 'b (mcons 'c (mcons 'd '())))

