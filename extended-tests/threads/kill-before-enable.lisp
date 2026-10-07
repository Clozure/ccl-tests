;;; kill-before-enable.lisp -- PROCESS-KILL on a process that has been preset
;;; but not yet enabled does not end it.
;;;
;;; PROCESS-KILL sends the kill as an interrupt.  A preset process that has
;;; not been enabled is in the :RESET state, and an interrupt to a :RESET
;;; thread does not run until something enables the thread.  So PROCESS-KILL
;;; returns T, but the process does not exit, its termination semaphore is
;;; never signalled, and it stays in ALL-PROCESSES.
;;;
;;; One thread, no race.  The file waits 5 s for the termination semaphore.
;;; run-all.sh still runs it under an external timeout, as it does every file
;;; here.
;;;
;;; Prints one result line and quits:
;;;   KILL-BEFORE-ENABLE-RESULT: PASS ...   exit 0
;;;   KILL-BEFORE-ENABLE-RESULT: FAIL ...   exit 42

(in-package :cl-user)

(defun report (ok fmt &rest args)
  (format t "~&KILL-BEFORE-ENABLE-RESULT: ~:[FAIL~;PASS~] ~?~%" ok fmt args)
  (finish-output)
  (ccl:quit (if ok 0 42)))

(let* ((sem (ccl:make-semaphore))
       (p (ccl:make-process "kill-before-enable"
                            :termination-semaphore sem)))
  (ccl:process-preset p (lambda () (sleep 1000)))
  ;; P is preset and not enabled, so it is in the :RESET state.
  (ccl:process-kill p)
  ;; The exiting thread signals its termination semaphore just BEFORE it
  ;; removes the process from ALL-PROCESSES, so give the removal 2 s.
  (let* ((exited (ccl:timed-wait-on-semaphore sem 5))
         (listed (loop repeat 20
                       while (member p (ccl:all-processes))
                       do (sleep 0.1)
                       finally (return (and (member p (ccl:all-processes)) t)))))
    (report (and exited (not listed))
            "exited within 5 s: ~s, still in ALL-PROCESSES: ~s"
            (and exited t) listed)))
