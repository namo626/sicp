#lang sicp

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;; ex 3.12
;;(define x (list 'a 'b))
;;(define y (list 'c 'd))
;;(define z (append x y))
;;(display z)
;;(display (cdr x))
;; >> (b)
;;(define w (append! x y))
;;(display w)
;;(display (cdr z))
;; >> (b c d)
