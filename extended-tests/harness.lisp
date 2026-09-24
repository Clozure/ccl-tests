;;;; extended-tests/harness.lisp -- assertions and a result line for the
;;;; extended tests.
;;;;
;;;; The runner asks a test for exactly two
;;;; things: a summary line
;;;;
;;;;     XT-RESULT <SUITE-NAME> TOTAL <n> FAILED <n>
;;;;
;;;; and an exit status that is 0 if and only if FAILED is 0.  Every test could
;;;; hand-roll that -- tools/ffi-transparent-union-test.lisp does, and it is the
;;;; model this file generalises -- but hand-rolling it once per test is how a
;;;; suite acquires four subtly different definitions of "failed".
;;;;
;;;; DELIBERATELY TINY AND DELIBERATELY PORTABLE.  This file must load in stock
;;;; x86-64 CCL 1.12.2 (the ORACLE) and in our arm64 image, so it uses nothing
;;;; but ANSI CL plus CCL:QUIT.  No target::, no ccl:: internals, no reader
;;;; conditionals.  The moment this file needs #+arm64-target it has stopped
;;;; being a harness and started being a test.
;;;;
;;;; WHY THE ORACLE MATTERS (the rule that makes the suite credible): a green-bar
;;;; test must PASS on stock x86-64 CCL.  If it fails there it is testing our
;;;; quirk or something implementation-defined -- not a gap in the standard --
;;;; and it does not belong in the green bar.
;;;;
;;;; Usage:
;;;;     (load "tools/beyond-ansi/harness.lisp")
;;;;     (xt-check "name" (some-form) expected)
;;;;     (xt-check "name" (some-form) expected :test #'string=)
;;;;     (xt-true  "name" (some-predicate))
;;;;     (xt-skip  "name" "why")
;;;;     (xt-report "MY-SUITE")            ; prints XT-RESULT and QUITS
;;;;
;;;; A case that SIGNALS is a FAILURE, never a crash of the run: BA-CHECK wraps
;;;; the form so one bad case cannot eat the cases after it.  That is the same
;;;; reason tools/box/arm-eval.sh predefines its PROBE macro -- without it, one
;;;; error means one fact per boot.

(in-package :cl-user)

(defvar *xt-total* 0)
(defvar *xt-failed* 0)
(defvar *xt-skipped* 0)

(defun xt-note (verdict name fmt &rest args)
  ;; One line per case, prefixed so the runner's log greps find them.  Flushed
  ;; every time: a buffered PASS that never reaches the log is indistinguishable
  ;; from a test that died before running.
  (format t "~&BA-CASE ~a ~a~@[ ~a~]~%" name verdict
          (and fmt (apply #'format nil fmt args)))
  (finish-output))

(defmacro xt-check (name form expected &key (test '#'eql))
  "Evaluate FORM; the case passes when its value matches EXPECTED under TEST."
  (let ((n (gensym)) (e (gensym)) (v (gensym)) (c (gensym)) (tf (gensym)))
    `(let ((,n ,name) (,e ,expected) (,tf ,test))
       (incf *xt-total*)
       (handler-case
           (let ((,v ,form))
             (if (funcall ,tf ,v ,e)
                 (xt-note "PASS" ,n "=> ~s" ,v)
                 (progn (incf *xt-failed*)
                        (xt-note "FAIL" ,n "got ~s want ~s" ,v ,e))))
         (error (,c)
           (incf *xt-failed*)
           ;; PRINC-TO-STRING can itself signal on a half-built condition, which
           ;; would turn a reported failure into a dead run.  Fall back to the
           ;; type name, then to a constant -- same defence arm-eval.sh's PROBE
           ;; macro uses.
           (xt-note "FAIL" ,n "signalled ~a"
                    (or (ignore-errors (princ-to-string ,c))
                        (ignore-errors (string (type-of ,c)))
                        "<unprintable condition>")))))))

(defmacro xt-true (name form)
  "Evaluate FORM; the case passes when its value is non-NIL."
  `(xt-check ,name (and ,form t) t))

(defun xt-skip (name why)
  "Record a case that could not run here.  A SKIP is NOT a pass: it is excluded
from TOTAL entirely, so it can never pad a green bar."
  (incf *xt-skipped*)
  (xt-note "SKIP" name "~a" why)
  nil)

;;; WHETHER A REPORT ENDS THE PROCESS IS THE SUITE'S CHOICE, NOT THE TEST'S.
;;; Each test file calls REPORT with no keyword, so the DEFAULT decides.  Left
;;; at T, loading ten test files runs exactly one of them: the first REPORT
;;; quits the image and the other nine never execute.  The suite loader binds
;;; this to NIL; a single file run on its own still exits with a status.
(defvar *report-quits* t)

(defun xt-report (suite &key (quit *report-quits*))
  "Print the runner's contract line and exit 0 iff nothing failed."
  (format t "~&XT-RESULT ~a TOTAL ~d FAILED ~d~@[ SKIPPED ~d~]~%"
          suite *xt-total* *xt-failed*
          (and (plusp *xt-skipped*) *xt-skipped*))
  (finish-output)
  (when quit
    (funcall (or (find-symbol "QUIT" "CCL") (find-symbol "QUIT" "CL-USER"))
             (if (zerop *xt-failed*) 0 1)))
  *xt-failed*)
