; Exercise 3.6: It is useful to be able to reset a random-number
; generator to produce a sequence starting from a given value.
; Design a new rand procedure that is called with an argument 
; that is either the symbol generate or the symbol
; reset and behaves as follows: 
; (rand 'generate) produces a new random number
; ((rand 'reset) ⟨new-value ⟩) resets the internal state variable to the designated ⟨new-value ⟩.
; Thus, by reseing the state, one can generate repeatable se-
; quences. These are very handy to have when testing and
; debugging programs that use random numbers.



(require rnrs)

(define (rand-update x)
  (let ((a 421)
        (c 1663)
        (m 7875))
    (mod (+ (* a x) c) m)))

(define random
  (let ((x 0))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define rand
  (let ((x 0))
    (λ(behave)
       (cond ((eq? 'generate behave)
              (set! x (rand-update x)) x)
             ((eq? 'reset behave)
              (λ(new-value)
                 (set! x new-value)))
             (else 'error)))))


; > (rand 'generate)
; 1663
; > (rand 'generate)
; 911
; > ((rand 'reset) 100)
; > (rand 'generate)
; 4388
; > 
; 

