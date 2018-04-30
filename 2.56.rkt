#lang racket

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv e var)
  (cond ((number? e) 0)
        ((variable? e) (if (same-variable? e var) 1 0))
        ((sum? e) (make-sum (deriv (addend e) var)
                            (deriv (augend e) var)))
        ((product? e)
         (make-sum
          (make-product (multiplier e)
                        (deriv (multiplicand e) var))
          (make-product (deriv (multiplier e) var)
                        (multiplicand e))))
        ((exponentiation? e)
         (make-product (make-product (exponent e)
                                     (make-exponentiation (base e)
                                                          (make-sum (exponent e) -1)))
                       (deriv (base e) var)))
        (else
         (error "UUNKNOWN" e))))

(define (make-sum a1 a2)
  (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2))
     (+ a1 a2))
    (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond
    ((or (=number? m1 0) (=number? m2 0)) 0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2)) (* m1 m2))
    (else (list '* m1 m2))))

(define (=number? e num)
  (and (number? e) (= e num)))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s)
  (if (= 3 (length s))
      (caddr s)
      (cons '+ (cddr s))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (if (= 3 (length p))
      (caddr p)
      (cons '* (cddr p))))

(define (exponentiation? e)
  (and (pair? e)
       (eq? (car e) '**)))

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (make-exponentiation b e)
  (cond
    ((=number? e 0) 1)
    ((=number? e 1) b)
    ((and (number? b) (number? e)) (expt b e))
    (else (list '** b e))))
