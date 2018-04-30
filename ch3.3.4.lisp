(defun half-adder (a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))


;; Ex 3.28

(defun or-gate (a1 a2 output)
  (labels ((or-action-procedure ()
             (let ((new-value
                    (logical-or (get-signal a1) (get-signal a2))))
               (after-delay or-gate-delay
                            #'(lambda ()
                                (set-signal! output new-value))))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok))

;; Ex 3.29
;; Using de Morgan's law
(defun or-gate-b (a1 a2 output)
  (let ((c (make-wire))
        (d (make-wire))
        (e (make-wire)))
    (inverter a1 c)
    (inverter a2 d)
    (and-gate c d e)
    (inverter e output)
    'ok))

;; delay = and-gate-delay + 2*inverter-delay

;; Ex 3.31
;; The initialization is necessary because set-signal! doesn't perform any operation if the new
;; signal is the same as the old signal. This means that the initial signals somewhere inside the
;; circuit would remain at 0 and not be updated when the simulation runs, if their "origins" are
;; unchanged. This leads to a different result.

;; Agenda
(defun make-time-segment (time queue)
  (cons time queue))
(defun segment-time (s)
  (car s))
(defun segment-queue (s)
  (cdr s))

(defun make-agenda (list 0))
(defun current-time (agenda) (car agenda))
(defun set-current-time! (agenda time)
  (setf (car agenda) time))
(defun segments (agenda) (cdr agenda))
(defun set-segments! (agenda segments)
  (setf (cdr agenda) segments))
(defun first-segment (agenda)
  (car (segments agenda)))
(defun rest-segments (agenda)
  (cdr (segments agenda)))

(defun empty-agenda? (agenda)
  (null (segments agenda)))


(defun add-to-agenda! (time action agenda)
  (labels ((belongs-before? (segments)
             (or (null segments)
                 (< time (segment-time (car segments)))))
           (make-new-time-segment (time action)
             (let ((q (make-queue)))
               (insert-queue! q action)
               (make-time-segment time q)))
           (add-to-segments! (segments)
             (if (= (segment-time (car segments)) time)
                 (insert-queue! (segment-queue (car segments))
                                action)
                 (let ((rest (cdr segments)))
                   (if (belongs-before? rest)
                       (setf (cdr segments)
                             (cons (make-new-time-segment time action)
                                   (cdr segments)))
                       (add-to-segments! rest))))))
    (let ((segments (segments agenda)))
      (if (belongs-before? segments)
          (set-segments! agenda
                         (cons (make-new-time-segment time action)
                               segments))
          (add-to-segments! segments)))))

(defun remove-first-agenda-item! (agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(defun first-agenda-item (agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))


;; Ex 3.32
;; The queue is necessary because the final output state must correspond to the final input state.
;; Depending on the order in which the inputs are changed, the sequence of changes for the output
;; can alternate between 0 and 1. In the last in, first out method, the final operation to be run
;; is the first in the timeline, and may not match the final in the timeline. In the and-gate example,
;; using the LIFO method and sequencing (B -> 0) before (A -> 1) will result in an output sequence of
;; (C -> 0 C -> 1) which ends up setting C to 1.
