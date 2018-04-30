#lang racket


(define (fast-expt b n)
  (define (I base expon acc)
    (cond
      ((= expon 0) acc)
      ((odd? expon)
       (I base (- expon 1) (* base acc)))
      (else
       (I (* base base) (/ expon 2) acc))))
  (I b n 1))
