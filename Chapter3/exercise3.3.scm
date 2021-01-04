; Modify the make-account procedure so that
; it creates password-protected accounts. That is, make-account
; should take a symbol as an additional argument

(define (make-account balance true-pass)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (cond ((and (eq? pass true-pass) (eq? m 'withdraw)) withdraw)
          ((and (eq? pass true-pass) (eq? m 'deposit)) deposit)
          ((not (eq? pass true-pass)) (λ (x) "Incorrect password"))
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

