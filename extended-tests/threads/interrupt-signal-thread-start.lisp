;;; interrupt-signal-thread-start.lisp -- the process-interrupt signal sent to
;;; the whole process while threads start.
;;;
;;; MECHANISM.  A new thread starts with the signal mask of the thread that
;;; created it, and the kernel used to set an empty mask before the thread had
;;; a TCR and a C stack area.  A process-directed signal can go to any thread
;;; that does not block it, so the kernel can give the process-interrupt
;;; signal (SIGPWR on Linux) to a thread in that window.  The handler then
;;; runs with no TCR or with no stack area, and on Linux x86-64 it faults:
;;; the lisp dies with SIGSEGV.
;;;
;;; The driver starts *THREADS* short threads, at most *MAX-LIVE* at a time.
;;; A sender thread calls kill(getpid(), SIGPWR) in a loop, so the kernel
;;; picks the thread that gets each signal.  No lisp code sends
;;; PROCESS-INTERRUPT; each signal finds an empty interrupt queue.  A
;;; watchdog reports FAIL if no thread is created for STALL-SECONDS.  A crash
;;; kills the process before it can print a result; run-all.sh reads the exit
;;; status and its external timeout is the last guard.
;;;
;;; Linux only.  On other platforms it prints PASS at once and exits 0.
;;;
;;; Prints one result line and quits:
;;;   INTERRUPT-SIGNAL-THREAD-START-RESULT: PASS ...   exit 0
;;;   INTERRUPT-SIGNAL-THREAD-START-RESULT: FAIL ...   exit 42
;;;
;;; expected-state linuxx8664: repro
;;; expected-state linuxarm64: clean

(in-package :cl-user)

(defparameter *threads* 20000)
(defparameter *max-live* 32)
(defparameter *stall-seconds* 30)
;;; SIGPWR on Linux, which is SIGNAL_FOR_PROCESS_INTERRUPT there.
(defparameter *signal* 30)

(defvar *slots* (ccl:make-semaphore))
(defvar *done* nil)
(defvar *progress* 0)
(defvar *signals* 0)

(defun report (ok fmt &rest args)
  (format t "~&INTERRUPT-SIGNAL-THREAD-START-RESULT: ~:[FAIL~;PASS~] ~?~%" ok fmt args)
  (finish-output)
  ;; QUIT stops every thread first, and stalled threads do not answer, so a
  ;; FAIL leaves through _exit.
  (if ok
    (ccl:quit 0)
    (ccl:external-call "_exit" :int 42 :void)))

#-linux-target
(report t "skipped: Linux only")

(defun watchdog ()
  (let ((last -1) (still 0))
    (loop
      (sleep 1)
      (when *done* (return))
      (if (= *progress* last) (incf still) (setq still 0 last *progress*))
      (when (>= still *stall-seconds*)
        (report nil "no thread created for ~d s at thread ~d, ~d signals sent"
                *stall-seconds* *progress* *signals*)))))

(defun sender ()
  (let ((pid (ccl:external-call "getpid" :int)))
    (loop until *done*
          do (ccl:external-call "kill" :int pid :int *signal* :int)
             (incf *signals*))))

(let ((sender nil))
  (ccl:process-run-function "watchdog" #'watchdog)
  (setq sender (ccl:process-run-function "sender" #'sender))
  (dotimes (i *max-live*) (ccl:signal-semaphore *slots*))
  (dotimes (i *threads*)
    (unless (ccl:timed-wait-on-semaphore *slots* (* 2 *stall-seconds*))
      (report nil "no thread exited for ~d s at thread ~d" (* 2 *stall-seconds*) i))
    (ccl:process-run-function
     (format nil "short-~d" i)
     (lambda ()
       (unwind-protect
            (length (make-list (+ 1 (mod i 50))))
         (ccl:signal-semaphore *slots*))))
    (setq *progress* (1+ i)))
  (setq *done* t)
  (ccl:process-wait-with-timeout
   "join" (* 30 ccl::*ticks-per-second*)
   (lambda () (ccl::process-exhausted-p sender)))
  (report (plusp *signals*) "~d threads, ~d signals sent" *threads* *signals*))
