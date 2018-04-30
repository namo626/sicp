#lang scheme

(define (make-pos row col)
  (cons row col))

(define (row-pos pos) (car pos))
(define (col-pos pos) (cdr pos))
(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (let ((kth-queen (make-pos new-row k)))
        (cons kth-queen rest-of-queens)))

(define (safe? k positions)
  (cond
    ((null? (cdr positions)) true)
    (else
     (let ((kth-queen (car positions)))
       (all (map (lambda (pos)
                   (cond
                     ((= (row-pos kth-queen) (row-pos pos))
                      false)
                     ((= 1 (abs (/ (- (row-pos kth-queen)
                                      (row-pos pos))
                                   (- k
                                      (col-pos pos)))))
                      false)
                     (else true)))
                 (cdr positions)))))))

(define (all preds)
  (cond
    ((null? preds) true)
    (else (and (car preds)
               (all (cdr preds))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
