(require rnrs)


(define (rand-update x)
  (let ((a 421)
        (c 1663)
        (m 7875))
    (mod (+ (* a x) c) m)))
(define rand (let ((x 0))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))


(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1)
                  trials-passed))))
  (iter trials 0))

(define (square x)
  (* x x))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (rectangle-square)
    (* (- x2 x1) (- y2 y1)))
  (define predicate (λ() (P x1 x2 y1 y2)))
  (* (rectangle-square) (monte-carlo trials predicate)))

(define (P x1 x2 y1 y2)
  (define (in-test x y x0 y0 R)
    (<= (+ (square (- x x0)) (square (- y y0)))
        (square R)))
  (define R
    (min (/ (- x2 x1) 2) (/ (- y2 y1) 2)))
  
  (in-test (random-in-range x1 x2) (random-in-range y1 y2) (+ x1 R) (+ y1 R) R))


; example:
; (estimate-integral P 0 10 0 10 1000)

; Use estimate-integral to produce an estimate of π by measuring the
; area of a unit circle.
(define PI (estimate-integral P 0 2 0 2 1000))



