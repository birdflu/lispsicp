; The encode procedure takes as arguments a
; message and a tree and produces the list of bits that gives
; the encoded message.
; encode-symbol is a procedure, that
; returns the list of bits that encodes a given symbol accord-
; ing to a given tree. It should design encode-symbol so
; that it signals an error if the symbol is not in the tree at all.
; Test your procedure by encoding the result you obtained in
; Exercise 2.67 with the sample tree and seeing whether it is
; the same as the original sample message.
(include "../utils.scm")
(include "exercise2.3.3.scm")
(include "exercise2.67.scm")

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
      (let ((l (left-branch tree))
            (r (right-branch tree)))
        (cond ((and (leaf? l) (equal? symbol (symbol-leaf l))) '(0))
              ((and (leaf? r) (equal? symbol (symbol-leaf r))) '(1))
              ((and (not (leaf? l)) (element-of-set? symbol (symbols l)))
               (cons 0 (encode-symbol symbol l)))
              ((and (not (leaf? r)) (element-of-set? symbol (symbols r)))
               (cons 1 (encode-symbol symbol r)))
              (else (error "symbol not found" symbol)))))

