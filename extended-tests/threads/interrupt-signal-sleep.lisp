;;; interrupt-signal-sleep.lisp -- the process-interrupt signal sent to the
;;; whole process while the main thread sleeps.
;;;
;;; MECHANISM.  When a thread takes an interrupt in lisp code, the kernel
;;; runs the lisp handler through callback_to_lisp.  That function sets
;;; tcr->save_vsp and tcr->save_fp to the interrupted stack and does not put
;;; back the old values.  On x86-64, _SPffcall and _SPcallback load %rsp
;;; and %rbp from these fields.  An interrupt at the pending-interrupt check
;;; at the end of _SPffcall leaves them wrong.  The thread then returns to
;;; NIL.
;;;
;;; A sender thread calls kill(getpid(), SIGPWR) in a loop for *RUN-SECONDS*.
;;; No lisp code sends PROCESS-INTERRUPT, so each signal finds an empty
;;; interrupt queue.  The main thread sleeps in short steps, so it is often
;;; in an ff-call when a signal comes.  A crash kills the process before it
;;; can print a result; run-all.sh reads the exit status and its external
;;; timeout is the last guard.
;;;
;;; Linux only.  On other platforms it prints PASS at once and exits 0.
;;;
;;; Prints one result line and quits:
;;;   INTERRUPT-SIGNAL-SLEEP-RESULT: PASS ...   exit 0
;;;   INTERRUPT-SIGNAL-SLEEP-RESULT: FAIL ...   exit 42
;;;
;;; expected-state linuxx8664: repro
;;; expected-state linuxarm64: clean

(in-package :cl-user)

(defparameter *run-seconds* 60)
;;; SIGPWR on Linux, which is SIGNAL_FOR_PROCESS_INTERRUPT there.
(defparameter *signal* 30)

(defvar *done* nil)
(defvar *signals* 0)

(defun report (ok fmt &rest args)
  (format t "~&INTERRUPT-SIGNAL-SLEEP-RESULT: ~:[FAIL~;PASS~] ~?~%" ok fmt args)
  (finish-output)
  (if ok
    (ccl:quit 0)
    (ccl:external-call "_exit" :int 42 :void)))

#-linux-target
(report t "skipped: Linux only")

(defun sender ()
  (let ((pid (ccl:external-call "getpid" :int)))
    (loop until *done*
          do (ccl:external-call "kill" :int pid :int *signal* :int)
             (incf *signals*))))

(let ((sender (ccl:process-run-function "sender" #'sender))
      (end (+ (get-internal-real-time)
              (* *run-seconds* internal-time-units-per-second))))
  (loop while (< (get-internal-real-time) end)
        do (sleep 0.01))
  (setq *done* t)
  (unless (ccl:process-wait-with-timeout
           "join" (* 30 ccl::*ticks-per-second*)
           (lambda () (ccl::process-exhausted-p sender)))
    (report nil "the sender did not exit, ~d signals sent" *signals*))
  (report (plusp *signals*) "~d s, ~d signals sent" *run-seconds* *signals*))
