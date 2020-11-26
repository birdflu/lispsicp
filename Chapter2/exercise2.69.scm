; The procedure  generate-huffman-tree takes as its argument 
; a list of symbol-frequency pairs (where no symbol
; appears in more than one pair) and generates a Huffman
; encoding tree according to the Huffman algorithm.
; Write successive-merge, using make-code-tree to successively 
; merge the smallest-weight elements
; of the set until there is only one element le, which is the
; desired Huffman tree. 
(include "../utils.scm")
(include "exercise2.3.4.scm")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;  ((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4)) =>
;
; ((leaf A 4)
;  ((leaf B 2) 
;   ((leaf D 1) 
;    (leaf C 1) 
;    (D C) 
;    2) 
;   (B D C) 
;    4)
;  (A B D C)
;  8)


(define (successive-merge pairs)
  (let ((first-leaf (car pairs))
        (second-leaf (cadr pairs)))
    (accumulate 
      make-code-tree 
      (list first-leaf second-leaf 
            (list (symbol-leaf first-leaf) (symbol-leaf second-leaf)) 
            (+ (weight-leaf first-leaf) (weight-leaf second-leaf))) 
      (reverse (cddr pairs)))))

