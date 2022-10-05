(define f
  (let ((first 'nill))
    (define (result x)
      (if (eq? first 'nill)
          (begin (set! first x) x)
          0))
    result))

; > (+ (f 1) (f 0))
; 1
; > (+ (f 0) (f 1))
; 0
; 
