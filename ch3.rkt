#lang scheme

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-account balance passwd)
  (define access-count 0)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch passcode m)
    (cond
      ((eq? passcode passwd)
       (set! access-count 0)
       (cond
         ((eq? m 'withdraw) withdraw)
         ((eq? m 'deposit) deposit)
         (else (error "Unknown request" m))))
      (else
       (cond
         ((= access-count 3)
          call-the-cops)
         (else
          (begin (set! access-count (+ 1 access-count))
                 (error "Incorrect password")))))))
  (define (call-the-cops x)
    "Cops were called")
  dispatch)

;; ex 3.7
(define (make-joint oldacc oldpass newpass)
  (define (new-dispatch pass m)
    (if (eq? pass newpass)
        (oldacc oldpass m)
        (error "Wrong password for account")))
  (cond
    ((oldacc oldpass 'withdraw) new-dispatch)
    (else (error "Wrong old password"))))

;; ex 3.1
(define (make-accumulator sum0)
  (define (accumulator incr)
    (begin (set! sum0 (+ sum0 incr))
           sum0))
  accumulator)

;; ex 3.2
(define (make-monitored f)
  (define counter 0)
  (define (mf sym)
    (cond
      ((eq? sym 'how-many-calls?) counter)
      ((eq? sym 'reset-count)
       (set! counter 0))
      (else (begin (set! counter (+ 1 counter))
                   (f sym)))))
  mf)


;;(define rand
;;  (let ((x random-init))
;;    (lambda ()
;;      (set! x (rand-update x))
;;      x)))
;; ex 3.6
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define rectangle-area
    (* (- x2 x1)
       (- y2 y1)))
  (define (area-test)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (exact->inexact (* (monte-carlo trials area-test)
                     rectangle-area)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      ((= trials-remaining 0)
       (/ trials-passed trials))
      ((experiment)
       (iter (- trials-remaining 1)
             (+ trials-passed 1)))
      (else
       (iter (- trials-remaining 1)
             trials-passed))))
  (iter trials 0))
;; area-test :: parameters -> True or False
;; takes area predicate and a rectangle, and tests whether or not
;; a random point (in the range of the rectangle) satisfies the
;; predicate

;; randomizer in range
(define (random-in-range low high)
  (let ((r (- high low)))
    (+ low (* (random) r))))

;; ex 3.6
;;(define (rand op)
;;  (cond
;;    ((eq? op 'generate)
;;     ...)
;;    ((eq? op 'reset)
;;     ...
;;     resetter)))

; ex 3.8
;; (+ (f 0) (f 1)) = 0 if left to right, otherwise 1
(define (make-f)
  (define env 0)
  (lambda (n)
    (cond
      ((and (= env 0) (= n 0))
       (begin
         (set! env 1)
         -1))
      ((and (= env 0) (= n 1))
       (begin
         (set! env 1)
         1))
      ((and (= env 1) (= n 1))
       (begin
         (set! env 0)
         n))
      ((and (= env 1) (= n 0))
       (begin
         (set! env 0)
         n)))))
