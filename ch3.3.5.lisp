;; 3.3.5 - Propagation of Constraints

(defun adder (a1 a2 sum)
  (labels ((process-new-value ()
             (cond
               ((and (has-value? a1) (has-value? a2))
                (set-value! sum
                            (+ (get-value a1) (get-value a2))
                            #'me))
               ((and (has-value? a1) (has-value? sum))
                (set-value! a2
                            (- (get-value sum) (get-value a1))
                            #'me))
               ((and (has-value? a2) (has-value? sum))
                (set-value! a1
                            (- (get-value sum) (get-value a2))
                            #'me))))
           (process-forget-value ()
             (forget-value! sum #'me)
             (forget-value! a1 #'me)
             (forget-value! a2 #'me)
             (process-new-value))
           (me (request)
             (cond
               ((eq request 'I-have-a-value) (process-new-value))
               ((eq request 'I-lost-my-value) (process-forget-value))
               (t (error "Unknown request: ADDER")))))
    (connect a1 #'me)
    (connect a2 #'me)
    (connect sum #'me)
    #'me))

(defun multiplier (m1 m2 product)
  (labels ((process-new-value ()
             (cond
               ((or (and (has-value? m1) (= (get-value m1) 0))
                    (and (has-value? m2) (= (get-value m2) 0)))
                (set-value! product 0 #'me))
               ((and (has-value? m1) (has-value? m2))
                (set-value! product
                            (* (get-value m1) (get-value m2))
                            #'me))
               ((and (has-value? m1) (has-value? product))
                (set-value! m2
                            (/ (get-value product) (get-value m1))
                            #'me))
               ((and (has-value? m2) (has-value? product))
                (set-value! m1
                            (/ (get-value product) (get-value m2))
                            #'me))))
           (process-forget-value ()
             (forget-value! product #'me)
             (forget-value! m1 #'me)
             (forget-value! m2 #'me)
             (process-new-value))
           (me (request)
             (cond
               ((eq request 'I-have-a-value) (process-new-value))
               ((eq request 'I-lost-my-value) (process-forget-value))
               (t (error "Unknown request: MULTIPLIER")))))
    (connect m1 #'me)
    (connect m2 #'me)
    (connect product #'me)
    #'me))

(defun constant (value connector)
  (labels ((me (request)
             (error "Unknown request: CONSTANT")))
    (connect connector #'me)
    (set-value! connector value #'me)
    #'me))

(defun probe (name connector)
  (labels ((print-probe (value)
             (princ ""))
           (process-new-value () (princ (get-value connector)))
           (process-forget-value () (princ '?))
           (me (request)
             (cond ((eq request 'I-have-a-value) (process-new-value))
                   ((eq request 'I-lost-my-value) (process-forget-value))
                   (t (error "Unknown request: PROBE")))))
    (connect connector #'me)
    #'me))


(defun inform-about-value (constraint)
  (funcall constraint 'I-have-a-value))
(defun inform-about-no-value (constraint)
  (funcall constraint 'I-lost-my-value))

;; Representing connectors
(defun make-connector ()
  (let ((value nil)
        (informant nil)
        (constraints '()))
    (labels ((set-my-value (newval setter)
               (cond
                 ((not (has-value? #'me))
                  (setf value newval)
                  (setf informant setter)
                  (for-each-except setter
                                   #'inform-about-value
                                   constraints))
                 ((not (= value newval))
                  (error "Contradiction"))
                 (t 'ignored)))
             (forget-my-value (retractor)
               (if (eq retractor informant)
                   (progn (setf informant nil)
                          (for-each-except retractor
                                           #'inform-about-no-value
                                           constraints))
                   'ignored))
             (connect (new-constraint)
               (if (not (member new-constraint constraints))
                   (setf constraints
                         (cons new-constraint constraints)))
               (if (has-value? #'me)
                   (inform-about-value new-constraint))
               'done)
             (me (request)
               (cond
                 ((eq request 'has-value?)
                  (if informant T NIL))
                 ((eq request 'value) value)
                 ((eq request 'set-value!) #'set-my-value)
                 ((eq request 'forget) #'forget-my-value)
                 ((eq request 'connect) #'connect)
                 (T (error "Unknown operation: CONNECTOR")))))
      #'me)))

(defun for-each-except (exception procedure list)
  (labels ((F (items)
             (cond
               ((null items) 'done)
               ((eq (car items) exception)
                (F (cdr items)))
               (T (funcall procedure (car items))
                  (F (cdr items))))))
    (F list)))

(defun has-value? (connector)
  (funcall connector 'has-value?))
(defun get-value (connector)
  (funcall connector 'value))
(defun set-value! (connector new-value informant)
  (funcall (funcall connector 'set-value!) new-value informant))
(defun forget-value! (connector retractor)
  (funcall (funcall connector 'forget) retractor))
(defun connect (connector new-constraint)
  (funcall (funcall connector 'connect) new-constraint))


;; ex 3.33
(defun averager (a b c)
  (let ((d (make-connector))
        (e (make-connector)))
    (adder a b d)
    (multiplier d e c)
    (constant 0.5 e)
    'ok))
