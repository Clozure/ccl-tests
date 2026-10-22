;;; interrupt-callback-lock-inversion.lisp -- a PROCESS-INTERRUPT function
;;; that takes a lock can deadlock against the callback table lock.
;;;
;;; MECHANISM.  The kernel delivers a process-interrupt through the callback
;;; dispatcher, and %LOOKUP-PASCAL-FUNCTION takes *CALLBACK-LOCK* on every
;;; delivery.  It takes the lock at the interrupt level of the interrupted
;;; thread, before the callback binds its own level.  The lock acquire runs
;;; inside WITHOUT-INTERRUPTS, and the unbind that ends it polls for a
;;; pending interrupt, so a thread can run an interrupt function while it
;;; holds *CALLBACK-LOCK*.  If that function waits for a lock L that a
;;; second thread holds, and the second thread is interrupted, the second
;;; thread's own delivery waits for *CALLBACK-LOCK*.  Neither thread can
;;; continue, and every later interrupt delivery waits with them.
;;;
;;; Worker threads take one lock in a loop.  Two interrupter threads call
;;; PROCESS-INTERRUPT on each worker with a function that takes the same
;;; lock.  An interrupter waits until its function has run before it sends
;;; the next one, so at most two interrupts are in flight per worker and the
;;; nesting depth stays small.  A watchdog that holds no lock counts the
;;; interrupt functions that ran; when the count stops for STALL-SECONDS it
;;; prints the state and the call history of every thread as a diagnostic
;;; and reports FAIL.  The run is bounded: RUN-SECONDS of work, then the
;;; result.  run-all.sh also runs it under an external timeout.
;;;
;;; Prints one result line and quits:
;;;   INTERRUPT-CALLBACK-LOCK-INVERSION-RESULT: PASS ...   exit 0
;;;   INTERRUPT-CALLBACK-LOCK-INVERSION-RESULT: FAIL ...   exit 42

(in-package :cl-user)

(defparameter *workers* 16)
(defparameter *interrupters* 2)
(defparameter *run-seconds* 60)
(defparameter *stall-seconds* 15)

(defvar *lock* (ccl:make-lock "work"))
(defvar *done* nil)
(defvar *work* 0)
(defvar *ran* 0)
(defvar *sent* 0)

(defun report (ok fmt &rest args)
  (format t "~&INTERRUPT-CALLBACK-LOCK-INVERSION-RESULT: ~:[FAIL~;PASS~] ~?~%" ok fmt args)
  (finish-output)
  ;; QUIT stops every thread first, and the stalled threads do not answer,
  ;; so a FAIL leaves through _exit.
  (if ok
    (ccl:quit 0)
    (ccl:external-call "_exit" :int 42 :void)))

(defun print-threads ()
  (dolist (p (ccl:all-processes))
    (format t "~&--- ~a  ~a~%" (ccl:process-name p) (ccl:process-whostate p)))
  (finish-output)
  (dolist (p (ccl:all-processes))
    (unless (eq p ccl:*current-process*)
      (format t "~&=== ~a~%" (ccl:process-name p))
      (finish-output)
      (ignore-errors (ccl:print-call-history :process p :count 16 :detailed-p nil))
      (finish-output))))

(defun worker ()
  (loop until *done*
        do (ccl:with-lock-grabbed (*lock*) (incf *work*))))

(defun interrupter (workers)
  (loop until *done*
        do (dolist (w workers)
             (when *done* (return))
             (let ((ran (ccl:make-semaphore)))
               (when (ignore-errors
                      (ccl:process-interrupt
                       w (lambda ()
                           (ccl:with-lock-grabbed (*lock*) (incf *ran*))
                           (ccl:signal-semaphore ran))))
                 (incf *sent*)
                 ;; Longer than the stall detector, which owns the report.
                 (ccl:timed-wait-on-semaphore ran (* 4 *stall-seconds*)))))))

(let* ((workers (loop for i below *workers*
                      collect (ccl:process-run-function
                               (format nil "worker-~d" i) #'worker)))
       (interrupters (loop for i below *interrupters*
                           collect (ccl:process-run-function
                                    (format nil "interrupter-~d" i)
                                    #'interrupter workers)))
       (last -1)
       (still 0))
  (dotimes (s *run-seconds*)
    (sleep 1)
    (if (= *ran* last) (incf still) (setq still 0 last *ran*))
    (when (>= still *stall-seconds*)
      (format t "~&STALL: ~d interrupt functions ran, ~d sent, no progress for ~d s~%"
              *ran* *sent* *stall-seconds*)
      (print-threads)
      (report nil "stalled after ~d interrupt functions" *ran*)))
  (setq *done* t)
  (ccl:process-wait-with-timeout
   "join" (* 10 ccl::*ticks-per-second*)
   (lambda () (notany #'ccl::process-active-p (append interrupters workers))))
  (report (plusp *ran*) "~d interrupt functions ran in ~d s" *ran* *run-seconds*))
