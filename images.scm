(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
    
(define (make-segment start end)
  (cons start end))

(define (polyline vertices)
  (if (null? (cdr vertices))
      '()
      (cons (make-segment (car vertices) (cadr vertices)) (polyline (cdr vertices)))))

(define (contour vertices)
  (polyline (append vertices (list (car vertices)))))

(define mark-of-zorro
  (let ((v1 (make-vect .1 .9))
        (v2 (make-vect .8 .9))
        (v3 (make-vect .1 .2))
        (v4 (make-vect .9 .6)))
    (segments->painter
     (list (make-segment v1 v2)
           (make-segment v2 v3)
           (make-segment v3 v4)))))

(define wave
  (let ((segs (append
               (polyline (list
                          (make-vect 0 0.15)
                          (make-vect 0.15 0.4)
                          (make-vect 0.3 0.35)
                          (make-vect 0.4 0.35)
                          (make-vect 0.35 0.15)
                          (make-vect 0.4 0)))
               (polyline (list
                          (make-vect 0.6 0)
                          (make-vect 0.65 0.15)
                          (make-vect 0.6 0.35)
                          (make-vect 0.75 0.35)
                          (make-vect 1 0.65)))
               (polyline (list
                          (make-vect 1 0.85)
                          (make-vect 0.6 0.55)
                          (make-vect 0.75 1)))
               (polyline (list
                          (make-vect 0.6 1)
                          (make-vect 0.5 0.7)
                          (make-vect 0.4 1)))
               (polyline (list 
                          (make-vect 0.25 1)
                          (make-vect 0.35 0.5)
                          (make-vect 0.3 0.4)
                          (make-vect 0.15 0.6)
                          (make-vect 0 0.35))))))
    (segments->painter segs)))
