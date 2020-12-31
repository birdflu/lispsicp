#lang scheme

(require rackunit)
(include "../exercise2.90.scm")

; polynomial
(install-polynomial-package)

;{polynomial x {0 5} {1 1} {2 2} {3 3}}
; 3x^3 + 2x^2 + 1x^1 + 5
(define a (make-polynomial 'x '((0 5) (1 1) (2 2) (3 3))))

;{polynomial x 3 2 1 5}
(define b (make-polynomial 'x '(3 2 1 5)))

(check-equal?
  (add a a)
  '{polynomial x {0 10} {1 2} {2 4} {3 6}}
  )

(check-equal?
  (add b b)
  '{polynomial x 6 4 2 10}
  )

(check-equal?
  (add a b)
  '{polynomial x {0 10} {1 2} {2 4} {3 6}}
  )

(check-equal?
  (add b a)
  '{polynomial x 6 4 2 10}
  )

(check-equal?
  (add b a b)
  '{polynomial x 9 6 3 15}
  )

(check-equal?
  (add a b a b)
  '{polynomial x {0 20} {1 4} {2 8} {3 12}}
  )

(check-equal?
  (add a (change-sign a))
  '{polynomial x}
  )

(check-equal?
  (=zero? (add a (change-sign a)))
  #t
  )

(check-equal?
  (add a (change-sign a))
  '{polynomial x}
  )

(check-equal?
  (=zero? (add a (change-sign a)))
  #t
  )

(check-equal?
  (mul a a)
  '{polynomial x {6 9} {5 12} {4 10} {3 34} {2 21} {1 10} {0 25}}
  )

(check-equal?
  (mul b b)
  '{polynomial x 9 12 10 34 21 10 25}
  )

(check-equal?
  (mul a b)
  '{polynomial x {6 9} {5 12} {4 10} {3 34} {2 21} {1 10} {0 25}}
  )

(check-equal?
  (mul b a)
  '{polynomial x 9 12 10 34 21 10 25}
  )

(check-equal?
  (mul a b a)
  '{polynomial
  x {9 27} {8 54} {7 63} {6 179} {5 201} {4 156} {3 286} {2 165} {1 75} {0 125}}
  )

(check-equal?
  (mul b a b)
  '{polynomial x 27 54 63 179 201 156 286 165 75 125}
  )

