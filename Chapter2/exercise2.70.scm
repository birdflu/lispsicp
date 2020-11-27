; The following eight-symbol alphabet with
; associated relative frequencies was designed to efficiently
; encode the lyrics of 1950s rock songs. (Note that the “sym-
; bols” of an “alphabet” need not be individual leers.)
; A 2
; GET 2
; SHA 3
; WAH 1
; BOOM 1
; JOB 2
; NA 16
; YIP 9
; Use generate-huffman-tree (Exercise 2.69) to generate a
; corresponding Huffman tree, and use encode (Exercise 2.68)
; to encode the following message:
; Get a job
; Sha na na na na na na na na
; Get a job
; Sha na na na na na na na na
; Wah yip yip yip yip yip yip yip yip yip
; Sha boom

(include "exercise2.69.scm")

(define tree
  (let ((sequence 
          '((a 2) (get 2) (sha 3) (wah 1) (boom 1) (job 2) (na 16) (yip 9))))
    (generate-huffman-tree sequence)))

(define message '(
                  get a job
                  sha na na na na na na na na
                  get a job
                  sha na na na na na na na na
                  wah yip yip yip yip yip yip yip yip yip
                  sha boom))

(define code (encode message tree))
(define decoded-message (decode code tree))

