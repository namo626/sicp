;; ex 3.13
(defun last-pair (x)
  (if (null (cdr x))
      x
      (last-pair (cdr x))))

(defun make-cycle (x)
  (setf (cdr (last x)) x)
  x)

;; make-cycle doesn't terminate

;; ex 3.14
;; (mystery x) gives back the reverse of x
;; v >> (a)

;; ex 3.16
(defun pair? (x)
  (cond
    ((null x) nil)
    ((listp x) t)
    (t nil)))

(defun count-pairs (x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))

;; returns 3
;; (defvar x '(a b c))
;; returns 4
;; (setf (car (cdr x)) (cdr (cdr x)))
;; returns 7
;; (setf (car x) (cdr x))
;; doesn't terminate
;; (setf x '(a b c))
;; (setf (car (cdr x)) x)

;; ex 3.17
(defun count-pairs-good (x)
  (let ((acc '()))
    (labels ((count-acc (x)
	       (cond
		 ((null x) 0)
		 ((not (pair? x)) 0)
		 ((member x acc) 0)
		 (t
		  (progn (setf acc (cons x acc))
			 (+ 1
			    (count-acc (car x))
			    (count-acc (cdr x))))))))
      (count-acc x))))

;; ex 3.18
(defun cycle? (list)
  (let ((acc '()))
    (labels ((cycle-check (list)
	       (cond
		 ((pair? list) nil)
		 ((member list acc) t)
		 (t (setf acc (cons list acc))
		    (cycle-check (cdr list))))))
      (cycle-check list))))


(defun front-ptr (queue) (car queue))
(defun rear-ptr (queue) (cdr queue))
(defun set-front-ptr! (queue item)
  (setf (car queue) item))
(defun set-rear-ptr! (queue item)
  (setf (cdr queue) item))
(defun empty-queue? (queue)
  (null (front-ptr queue)))
(defun make-queue () (cons '() '()))

(defun front-queue (queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue")
      (car (front-ptr queue))))

(defun insert-queue! (queue item)
  (let ((new-pair (cons item '())))
    (cond
      ((empty-queue? queue)
       (set-front-ptr! queue new-pair)
       (set-rear-ptr! queue new-pair)
       queue)
      (t
       (setf (cdr (rear-ptr queue)) new-pair)
       (set-rear-ptr! queue new-pair)
       queue))))

(defun delete-queue! (queue)
  (cond
    ((empty-queue? queue)
     (error "DELETE! called with an empty queue"))
    (t
     (set-front-ptr! queue (cdr (front-ptr queue)))
     queue)))

;; ex 3.21
(defun print-queue (queue)
  (princ (front-ptr queue)))

;; ex 3.22 - representation of queue as function with local state
(defun make-queue-st ()
  (let ((front-ptr nil)
        (rear-ptr nil))
    (labels ((set-rear-ptr (item)
               (setf rear-ptr item))
             (set-front-ptr (item)
               (setf front-ptr item))
             (get-front ()
               front-ptr)
             (get-rear ()
               rear-ptr)
             (empty-queuep ()
               (null front-ptr))
             (insert-queue (item)
               (let ((new-pair (cons item '())))
                 (cond
                   ((empty-queuep)
                    (set-front-ptr new-pair)
                    (set-rear-ptr new-pair))
                   (t
                    (setf (cdr rear-ptr) new-pair)
                    (set-rear-ptr new-pair)))))
             (delete-queue ()
               (cond
                 ((empty-queuep)
                  (error "Cannot delete an empty queue"))
                 (t (set-front-ptr (cdr front-ptr)))))
             (print-queue ()
               (princ front-ptr))
             (dispatch (m)
               (cond
                 ((eq m 'set-rear-ptr) #'set-rear-ptr)
                 ((eq m 'set-front-ptr) #'set-front-ptr)
                 ((eq m 'get-front) #'get-front)
                 ((eq m 'get-rear) #'get-rear)
                 ((eq m 'empty-queuep) #'empty-queuep)
                 ((eq m 'insert-queue) #'insert-queue)
                 ((eq m 'delete-queue) #'delete-queue)
                 ((eq m 'print-queue) #'print-queue))))
      #'dispatch)))



;;; Representing tables
;; ex 3.24
(defun make-table (same-key?)
  (let ((local-table (list '*table*)))
    (labels ((lookup (key-1 key-2)
               (let ((subtable
                      (assoc1 key-1 (cdr local-table) same-key?)))
                 (if subtable
                     (let ((record
                            (assoc1 key-2 (cdr subtable) same-key?)))
                       (if record (cdr record) nil))
                     nil)))
             (insert! (key-1 key-2 value)
               (let ((subtable
                      (assoc1 key-1 (cdr local-table) same-key?)))
                 (if subtable
                     (let ((record
                            (assoc1 key-2 (cdr subtable) same-key?)))
                       (if record
                           (setf (cdr record) value)
                           (setf (cdr subtable)
                                 (cons (cons key-2 value)
                                       (cdr subtable)))))
                     (setf (cdr local-table)
                           (cons (list key-1 (cons key-2 value))
                                 (cdr local-table)))))
               'ok)
             (dispatch (m)
               (cond
                 ((eql m 'lookup-proc) #'lookup)
                 ((eql m 'insert-proc!) #'insert!)
                 (t (error "Unknown operation: TABLE")))))
      #'dispatch)))

(defun assoc1 (key records test)
  (cond
    ((null records) nil)
    ((funcall test key (car (car records)))
     (car records))
    (t (assoc1 key (cdr records) test))))

(defvar operation-table (make-table #'equal))
(defvar get1 (funcall operation-table 'lookup-proc))
(defvar put1 (funcall operation-table 'insert-proc!))


;; ex 3.25
(defun lookup-nth (keys table)
  (cond
    ((null keys) nil)
    (t
     (let ((subtable (assoc1 (car keys) (cdr table) #'equal)))
       (if subtable
           (if (null (cdr keys))
               (cdr subtable)
               (lookup-nth (cdr keys) subtable))
           nil)))))

(defun insert-nth (keys value table)
  (cond
    ((null keys) nil)
    (t
     (let ((subtable (assoc1 (car keys) (cdr table) #'equal)))
       (if subtable
           (if (null (cdr keys)) ; is this the last key?
               (setf (cdr subtable) value)
               (insert-nth (cdr keys) value subtable))
           (setf (cdr table)
                 (cons (make-nested keys value)
                       (cdr table))))))))

(defun make-nested (keys value)
  (cond
    ((null (cdr keys)) ;;; if this is the last key, make a pair
     (cons (car keys) value))
    (t
     (cons (car keys)
           (list (make-nested (cdr keys) value))))))
