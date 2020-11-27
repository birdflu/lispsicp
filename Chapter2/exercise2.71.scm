#lang scheme
; Suppose we have a Huffman tree for an al-
; phabet of n symbols, and that the relative frequencies of
; the symbols are 1, 2, 4, . . . , 2^(n −1) . Sketch the tree for n = 5;
(include "exercise2.69.scm")

;
;      ABCDE 31 
;     / \
; A 16   BCDE 15      ; A 16
;       / \           ; B  8 
;    B 8   CDE 7      ; C  4
;         / \         ; D  2
;      C 4   DE 3     ; E  1
;           / \
;        D 2   E 1
;
; In such a tree (for general n) how many bits
; are required to encode the most frequent symbol?
; answer: 1

; The least frequent symbol?
; answer: n - 1

(define tree
  (let ((sequence
          '((A 16) (B 8) (C 4) (D 2) (E 1))))
    (generate-huffman-tree sequence)))

(define codeA (encode '(A) tree))
(define codeE (encode '(E) tree))

(define (generate-sequence max-n)
  (define (gen-seq n)
    (if (= n max-n)
        (list (list n (expt 2 n)))
        (cons (list n (expt 2 n)) (gen-seq (+ 1 n)))))
  (gen-seq 0))

(define (answer n)
  (let ((sequence (generate-sequence n)))
    (let ((tree (generate-huffman-tree sequence))
          (minElement (list (car (car sequence))))
          (maxElement (list (car (car (reverse sequence))))))
      (display "sequence : ") (display sequence)
      (newline)
      (display "code max frequrency= ") (display (encode maxElement tree))
      (display ", len=") (display (length (encode maxElement tree)))
      (newline)
      (display "code min frequrency= ") (display (encode minElement tree))
      (display ", len=") (display (length (encode minElement tree)))
      (newline)
      )))

(display "sequence : ") (display '((A 16) (B 8) (C 4) (D 2) (E 1)))
(newline)
(display "code max frequrency= ") (display codeA)
(display ", len=") (display (length codeA))
(newline)
(display "code min frequrency= ") (display codeE)
(display ", len=") (display (length codeE))
(newline)

(newline)
(answer 4)
(newline)
(answer 9)
