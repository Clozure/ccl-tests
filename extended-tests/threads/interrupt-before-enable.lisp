;;; interrupt-before-enable.lisp -- PROCESS-INTERRUPT on a process that has
;;; been preset but not yet enabled leaves PROCESS-ENABLE unable to enable it.
;;;
;;; MECHANISM.  A preset process that has not been enabled is in the :RESET
;;; state.  THREAD-INTERRUPT's :RESET arm presets the thread again and calls
;;; THREAD-ENABLE, which takes the thread's one reset-completion token.  It
;;; does not signal the thread's activation semaphore, as PROCESS-ENABLE does
;;; through PROCESS-TCR-ENABLE.  So the thread never runs, never gives the
;;; token back, and the creator's PROCESS-ENABLE waits for it.
;;;
;;; One thread, no storm.  PROCESS-ENABLE gets a 2 s timeout here, so a stuck
;;; enable signals its error and this file reports it.  run-all.sh still runs
;;; it under an external timeout, as it does every file here.
;;;
;;; Prints one result line and quits:
;;;   INTERRUPT-BEFORE-ENABLE-RESULT: PASS ...   exit 0
;;;   INTERRUPT-BEFORE-ENABLE-RESULT: FAIL ...   exit 42

(in-package :cl-user)

(defvar *interrupt-ran* nil)
(defvar *initial-ran* nil)
(defvar *done* (ccl:make-semaphore))

(defun report (ok fmt &rest args)
  (format t "~&INTERRUPT-BEFORE-ENABLE-RESULT: ~:[FAIL~;PASS~] ~?~%" ok fmt args)
  (finish-output)
  (ccl:quit (if ok 0 42)))

(let ((p (ccl:make-process "interrupt-before-enable")))
  (ccl:process-preset p (lambda ()
                          (setq *initial-ran* t)
                          (ccl:signal-semaphore *done*)))
  ;; P is preset and not enabled, so this takes THREAD-INTERRUPT's :RESET arm.
  (ccl:process-interrupt p (lambda () (setq *interrupt-ran* t)))
  (handler-case (ccl:process-enable p 2)
    (error (c)
      (report nil "PROCESS-ENABLE did not enable the process: ~a" c)))
  (unless (ccl:timed-wait-on-semaphore *done* 5)
    (report nil "the initial function did not run within 5 s (interrupt ran: ~s)"
            *interrupt-ran*))
  (report (and *interrupt-ran* *initial-ran*)
          "interrupt ran: ~s, initial function ran: ~s"
          *interrupt-ran* *initial-ran*))
