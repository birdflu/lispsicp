; Modify the make-account procedure of Exercise 3.3 
; by adding another local state variable so that, if
; an account is accessed more than seven consecutive times
; with an incorrect password, it invokes the procedure 
; call-the-cops.

(define (make-account balance true-pass)
  (let ((counter 0))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define call-the-cops (λ(x) "Call the cops"))
  (define (dispatch pass m)
    (cond ((and (eq? pass true-pass) (eq? m 'withdraw)) withdraw)
          ((and (eq? pass true-pass) (eq? m 'deposit)) deposit)
          ((= counter 7) call-the-cops)
          ((not (eq? pass true-pass)) (λ (x) (begin (set! counter (+ counter 1)) "Incorrect password")))
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch))


