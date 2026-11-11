;;; interrupt-spinning-thread.lisp -- a PROCESS-INTERRUPT to a thread that
;;; spins can be lost, and then every later one to that thread is lost too.
;;;
;;; MECHANISM.  A second interrupt can arrive after the interrupt callback
;;; returns but before the signal handler exits.  The thread is then in
;;; foreign code, so the handler only marks the interrupt pending.  Nothing
;;; raises it again, and a thread that runs on at interrupt level 0 without
;;; polling never takes it.  PROCESS-INTERRUPT sends a signal only when no
;;; interrupt is pending, so every later call just queues its function.
;;;
;;; One thread spins.  The main thread sends PROCESS-INTERRUPT with a
;;; function that signals a semaphore, and waits for it, in a loop.  When a
;;; function has not run after STALL-SECONDS, the file reports FAIL.
;;; Otherwise it reports PASS after RUN-SECONDS.  run-all.sh also runs it
;;; under an external timeout.
;;;
;;; Prints one result line and quits:
;;;   INTERRUPT-SPINNING-THREAD-RESULT: PASS ...   exit 0
;;;   INTERRUPT-SPINNING-THREAD-RESULT: FAIL ...   exit 42
;;;
;;; expected-state linuxx8664: repro
;;; expected-state linuxarm64: repro

(in-package :cl-user)

(defparameter *run-seconds* 60)
(defparameter *stall-seconds* 10)

(defvar *stop* nil)
(defvar *spin* 0)

(defun report (ok fmt &rest args)
  (format t "~&INTERRUPT-SPINNING-THREAD-RESULT: ~:[FAIL~;PASS~] ~?~%" ok fmt args)
  (finish-output)
  ;; QUIT stops every thread first, and the spinner does not take
  ;; interrupts in the failing case, so a FAIL leaves through _exit.
  (if ok
    (ccl:quit 0)
    (ccl:external-call "_exit" :int 42 :void)))

(defun main ()
  (let* ((sem (ccl:make-semaphore))
         (w (ccl:process-run-function "spin"
              (lambda () (loop until *stop* do (incf *spin*)))))
         (count 0)
         (end (+ (get-internal-real-time)
                 (* *run-seconds* internal-time-units-per-second))))
    (sleep 0.2)
    (loop while (< (get-internal-real-time) end)
          do (ccl:process-interrupt w (lambda () (ccl:signal-semaphore sem)))
             (unless (ccl:timed-wait-on-semaphore sem *stall-seconds*)
               (report nil "no interrupt function ran for ~d s after ~d round trips"
                       *stall-seconds* count))
             (incf count))
    (setq *stop* t)
    (report t "~d round trips in ~d s, spin ~d" count *run-seconds* *spin*)))

(main)
