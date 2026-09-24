;;; sleep-vs-alloc.lisp -- (SLEEP n) returns far too late, or does not return,
;;; when another thread allocates large objects.  Upstream issue #639.
;;;
;;; MECHANISM.  Each GC suspends the sleeping thread with a signal.  That
;;; interrupts #_nanosleep, which returns EINTR, and %nanosleep then sleeps
;;; again for the remaining time THE KERNEL REPORTED.  The kernel computes
;;; that remainder before it runs the handler, so the time the thread spends
;;; parked in suspend_resume_handler -- waiting for the collection to finish
;;; -- is never subtracted.  Every world stop loses its own duration.
;;;
;;; So the error is not a fixed offset.  It grows with the collection RATE,
;;; and the sleep converges only when it gains time faster than it loses it.
;;; The same divergence happens in C with no CCL: an EINTR-and-re-sleep loop
;;; under a signal storm converges when the handler returns at once and
;;; diverges when the handler parks for as little as 16 us.  The BLOCKING
;;; HANDLER is the variable, not the signal rate.
;;;
;;; ### What this file has actually been observed to do
;;;
;;;   linuxx8664  t3.small, 2 vCPU    10 s sleep took  29.4 s     682 k allocs
;;;   linuxarm64  t4g.small, 2 vCPU   10 s sleep took 123.0 s    4.6 M allocs
;;;   linuxarm64  m9g.large           had not returned when killed at 90 s, 3/3
;;;
;;; ⚠ IT IS NOT ESTABLISHED THAT THE SLEEP NEVER RETURNS.  The unbounded
;;; reading comes from runs that were KILLED, and on a slower box the same
;;; sleep did return, after 12x its requested time.  What is measured is that
;;; the overrun grows without a bound anyone has found, not that it is
;;; infinite.  This file reports the number it saw and does not decide that.
;;;
;;; Because the overrun scales with collection rate, a THROTTLED or low-core
;;; machine reproduces this MORE WEAKLY, not more strongly: fewer collections
;;; per second means less time lost per second.  Both numbers above come from
;;; burstable instances and are therefore conservative.
;;;
;;; ### Three things in this file are load-bearing
;;;
;;; 1. THE ALLOCATED VALUE MUST BE USED.  CCL removes an allocation whose
;;;    value is discarded.  Drop the SETQ and the counter climbs past a
;;;    billion, no GC happens, and the sleep returns on time -- a green run
;;;    that has tested nothing.
;;;
;;; 2. THE WATCHDOG RUNS IN THE CONSING THREAD.  Every sleep primitive in the
;;;    image goes through the code under test, so a watchdog that sleeps
;;;    cannot be trusted to wake on time; it would be late by the mechanism it
;;;    is timing.  The conser is runnable throughout, so it is the only thread
;;;    here that can hold a clock.
;;;
;;; 3. EXACTLY ONE THREAD MAY REPORT.  QUIT from the watchdog unwinds the
;;;    sleeping thread OUT of its sleep, so that thread then runs its own
;;;    reporting path and, before this was fixed, printed a SECOND result line
;;;    with a different elapsed time and a different exit code.  A reader and
;;;    a runner both got two answers.  FINISH is therefore guarded, and the
;;;    loser exits with the WINNER's code.
;;;
;;; Contract:  SLEEP-VS-ALLOC-RESULT :REQUESTED r :ELAPSED e :RATIO x
;;;                                  :ALLOCATIONS n :VERDICT PASS | FAIL
;;; Exit 0 when the sleep returns within tolerance, 42 when it returns late,
;;; 43 when it had not returned by the watchdog deadline.

(in-package :cl-user)

(defparameter *requested* 10
  "Seconds to ask SLEEP for.")

(defparameter *tolerance* 20
  "Twice the requested time.  A correct SLEEP returns at about 10.0 s, so this
   is far above scheduling noise, and far below every overrun measured above.")

(defparameter *watchdog* 180
  "Give up here.  Must exceed the worst late-but-returning run, so that `had
   not returned' stays distinguishable from `returned very late'.")

(defvar *keep* nil "Holds the last allocation so the compiler cannot elide it.")
(defvar *allocs* 0)
(defvar *stop* nil)
(defvar *t0* nil)
(defvar *report-lock* (ccl:make-lock))
(defvar *reported* nil)
(defvar *exit-code* 0)

(defun secs-since (start)
  (float (/ (- (get-internal-real-time) start)
            internal-time-units-per-second)))

(defun finish (elapsed verdict code &optional note)
  "Report at most once, then exit with whatever code the FIRST caller set."
  (ccl:with-lock-grabbed (*report-lock*)
    (unless *reported*
      (setq *reported* t
            *exit-code* code)
      (when note
        (format t "~&SLEEP-VS-ALLOC: ~a~%" note))
      (format t "~&SLEEP-VS-ALLOC-RESULT :REQUESTED ~d :ELAPSED ~,1F ~
                 :RATIO ~,1F :ALLOCATIONS ~d :VERDICT ~a~%"
              *requested* elapsed (/ elapsed *requested*) *allocs* verdict)
      (force-output)))
  (ccl:quit *exit-code*))

(defun conser ()
  (loop until *stop*
        do (setq *keep* (make-array 20000))     ; 160016 bytes
           (incf *allocs*)
           (when (> (secs-since *t0*) *watchdog*)
             (finish (secs-since *t0*) "FAIL" 43
                     (format nil "the sleep had not returned after ~d s. ~
                                  Requested ~d s." *watchdog* *requested*)))))

(setq *t0* (get-internal-real-time))
(ccl:process-run-function "conser" #'conser)

;; No warm-up sleep: a sleep here would be subject to the defect under test.
;; The conser allocates within microseconds and the window is 10 s.
(sleep *requested*)

(let ((elapsed (secs-since *t0*)))
  (setq *stop* t)
  (if (<= elapsed *tolerance*)
      (finish elapsed "PASS" 0)
      (finish elapsed "FAIL" 42)))
