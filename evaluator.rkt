#lang racket
(require rnrs/mutable-pairs-6)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval-and (preds-body exp) env))
        ((or? exp) (eval-or (preds-body exp) env))
        ((let? exp) (eval (let->combination exp) env))     ;; Ex 4.6
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure-body)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;;; Representing Expressions

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caadr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? cdr seq))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operand ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))

;; Modified in Ex 4.5
;; (test => recipient)
(define (cond-arrow-clause? clause)
  (eq? '=> (cadr clause)))
(define (cond-arrow-recipient clause)
  (caddr clause))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond
          ((cond-else-clause? first)
           (if (null? rest)
               (sequence->exp (cond-actions first))
               (error "ELSE" clauses)))
          ((cond-arrow-clause? first)
           (make-if (cond-predicate first)
                    (cons (cond-arrow-recipient first)
                          (list (cond-predicate first)))
                    (expand-clauses rest)))
          ((make-if (cond-predicate first)
                    (sequence->exp (cond-actions first))
                    (expand-clauses rest)))))))



;; Ex 4.4
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (preds-body exp) (cdr exp))
(define (last-pred? body) (null? (cdr body)))

(define (eval-and body env)
  (cond
    ((null? body) 'true)
    ((true? (eval (car body) env))
     (if (last-pred? body)
         (eval (car body) env)
         (eval-and (cdr body) env)))
    (else 'false)))

(define (eval-or body env)
  (cond
    ((null? body) 'false)
    ((true? (eval (car body) env))
     (eval (car body) env))
    (else (eval-or (cdr body) env))))

;; Ex 4.6
(define (let? exp) (tagged-list? exp 'let))
(define (let-vars exp)
  (map car (cadr exp)))
(define (let-exps exp)
  (map cadr (cadr exp)))
(define (let-body exp)
  (cddr exp))
(define (let->combination exp)
  (cons (make-lambda (let-vars exp)
                     (let-body exp))
        (let-exps exp)))

;;; Evaluator data structures - functions within the evaluator itself; they process the "external" syntax, that is, the input language to the evaluator

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

;; Compound procedures differ from lambdas in that they contain the environment
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; Environment functions - an env is a list of frames; each frame contains variables and their definitions
(define (enclosing-environment env) (mcdr env))
(define (first-frame env) (mcar env))
(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables values))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-cdr! frame (mcons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (mcons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;; This uses mutual recursion
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond
        ((null? vars)
         (env-loop (enclosing-environment env)))
        ((eq? var (mcar vars)) (mcar vals))
        (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))

  (env-loop env))

(define (env-general var env action env-action)
  (define (env-loop env)
    (define (scan vars vals)
      (cond
        ((null? vars)
         (env-loop (enclosing-environment env)))
        ((eq? var (mcar vars))
         (action vals))
        (else (scan (cdr vars)
                    (cdr vals)))))
    (if (eq? env the-empty-environment)
        (env-action)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (lookup var env)
  (env-general var
               env
               (lambda (vals)
                 (mcar vals))
               (lambda ()
                 (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (env-general var
               env
               (lambda (vals)
                 (set-mcar! vals val))
               (lambda ()
                 (error "Unbound variable: SET!" var))))
