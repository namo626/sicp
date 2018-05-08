#lang racket
(require scheme/mpair)


;; Environment functions - an env is a list of frames; each frame contains variables and their definitions
(define (enclosing-environment env) (mcdr env))
(define (first-frame env) (mcar env))
(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables values))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

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
        (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))

  (env-loop env))

(define (env-general var env action frame-action env-action)
  (define (env-loop env)
    (define (scan vars vals)
      (cond
        ((null? vars)
         (frame-action env-loop env))
         ;;(env-loop (enclosing-environment env)))
        ((eq? var (mcar vars))
         (action vals))
        (else (scan (mcdr vars)
                    (mcdr vals)))))
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
               (lambda (f e)
                 (f (enclosing-environment e)))
               (lambda ()
                 (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (env-general var
               env
               (lambda (vals)
                 (set-mcar! vals val))
               (lambda (f e)
                 (f (enclosing-environment e)))
               (lambda ()
                 (error "Unbound variable: SET!" var))))

(define (define-variable! var val env)
  (env-general var
               env
               (lambda (vals)
                 (set-mcar! vals val))
               (lambda (f e)
                 (add-binding-to-frame! var
                                        val
                                        (first-frame e)))
               (lambda () 'nil)))
