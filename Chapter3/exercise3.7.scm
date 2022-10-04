; Consider the bank account objects created by
; make-account, with the password modification described
; in Exercise 3.3. Suppose that our banking system requires
; the ability to make joint accounts. Define a procedure make-
; joint that accomplishes this. make-joint should take three
; arguments. e first is a password-protected account. e
; second argument must match the password with which the
; account was defined in order for the make-joint operation
; to proceed. e third argument is a new password. make-
; joint is to create an additional access to the original ac-
; count using the new password. For example, if peter-acc
; is a bank account with password open-sesame, then
; (define paul-acc
; (make-joint peter-acc 'open-sesame 'rosebud))
; will allow one to make transactions on peter-acc using the
; name paul-acc and the password rosebud. You may wish
; to modify your solution to Exercise 3.3 to accommodate this
; new feature.


(define (make-account balance true-pass)
  (let ((account-pass 'nill))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (set-pass pass)
      (set!  account-pass pass)
      (list pass "Password is set"))
    (define (dispatch pass m)
      (cond ((and 
               (or (eq? pass true-pass) (eq? pass account-pass)) 
               (eq? m 'withdraw)) 
             withdraw)
            ((and 
               (or (eq? pass true-pass) (eq? pass account-pass))
               (eq? m 'deposit)) 
             deposit)
            ((and 
              (or (eq? pass true-pass) (eq? pass account-pass))  
              (eq? m 'set-pass)) 
             set-pass) 
            ((not 
               (or (eq? pass true-pass) (eq? pass account-pass)))
             (λ (x) "Incorrect password"))
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    (set! account-pass true-pass)
    dispatch))

(define (make-joint account pass new-pass)
  ((account pass 'set-pass) new-pass)
  account)
  
; > (include "Chapter3/exercise3.7.scm")
; > (define peter-acc (make-account 100 'open-sesame))
; > ((peter-acc 'open-sesame 'withdraw) 1)
; 99
; > (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
; > ((paul-acc 'rosebud 'withdraw) 1)
; 98

