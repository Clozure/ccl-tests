;;; interrupt-storm-short-threads.lisp -- PROCESS-INTERRUPT sent without a
;;; pause at threads that start, run for a moment and exit.
;;;
;;; MECHANISM.  An interrupt function that waits for a lock waits in foreign
;;; code.  A process-interrupt signal that arrives there is deferred, and when
;;; the wait returns the thread runs the deferred delivery.  If the delivery
;;; can start while an earlier one is still on the stack, each new interrupt
;;; adds a nested callback, about 9 KB of C stack on Linux x86-64.  Under a
;;; storm the nesting runs off the end of the C stack and the signal handler
;;; faults: the lisp dies with SIGSEGV, or, on linuxarm64, threads stop
;;; making progress.
;;;
;;; The driver starts *THREADS* short threads, at most *MAX-LIVE* at a time.
;;; Two interrupter threads send PROCESS-INTERRUPT to every short thread that
;;; is in :RUN, as fast as they can, with a function that takes a lock.  A
;;; watchdog that holds no lock reports FAIL if no thread is created for
;;; STALL-SECONDS.  A crash kills the process before it can print a result;
;;; run-all.sh reads the exit status and its external timeout is the last
;;; guard.
;;;
;;; Prints one result line and quits:
;;;   INTERRUPT-STORM-SHORT-THREADS-RESULT: PASS ...   exit 0
;;;   INTERRUPT-STORM-SHORT-THREADS-RESULT: FAIL ...   exit 42
;;;
;;; expected-state linuxx8664: repro
;;; expected-state linuxarm64: repro

(in-package :cl-user)

(defparameter *threads* 20000)
(defparameter *max-live* 32)
(defparameter *interrupters* 2)
(defparameter *stall-seconds* 30)

(defvar *slots* (ccl:make-semaphore))
(defvar *count-lock* (ccl:make-lock "count"))
(defvar *done* nil)
(defvar *progress* 0)
(defvar *sent* 0)
(defvar *ran* 0)

(defun report (ok fmt &rest args)
  (format t "~&INTERRUPT-STORM-SHORT-THREADS-RESULT: ~:[FAIL~;PASS~] ~?~%" ok fmt args)
  (finish-output)
  ;; QUIT stops every thread first, and stalled threads do not answer, so a
  ;; FAIL leaves through _exit.
  (if ok
    (ccl:quit 0)
    (ccl:external-call "_exit" :int 42 :void)))

(defun thread-state (p)
  ;; The slot, read without its lock: THREAD-STATE takes the state-change
  ;; lock, which a stalled thread can hold.
  (ignore-errors (ccl::lisp-thread.state (ccl::process-thread p))))

(defun watchdog ()
  (let ((last -1) (still 0))
    (loop
      (sleep 1)
      (when *done* (return))
      (if (= *progress* last) (incf still) (setq still 0 last *progress*))
      (when (>= still *stall-seconds*)
        (format t "~&STALL: no thread created for ~d s at thread ~d, ~d interrupts sent~%"
                *stall-seconds* *progress* *sent*)
        (dolist (p (ccl:all-processes))
          (format t "~&--- ~a  ~s~%" (ccl:process-name p) (thread-state p)))
        (finish-output)
        (report nil "stalled at thread ~d of ~d" *progress* *threads*)))))

(defun interrupter ()
  (loop until *done*
        do (dolist (p (ccl:all-processes))
             (when (and (eql 0 (search "short-" (ccl:process-name p)))
                        (eq :run (thread-state p)))
               ;; PROCESS-INTERRUPT returns NIL when it queues the function
               ;; but sends no new signal, and the function still runs.  So
               ;; a send is any call that does not signal an error.
               (when (handler-case
                         (progn
                           (ccl:process-interrupt
                            p (lambda ()
                                (ccl:with-lock-grabbed (*count-lock*) (incf *ran*))))
                           t)
                       (error () nil))
                 (ccl:with-lock-grabbed (*count-lock*) (incf *sent*)))))))

(let ((ints '()))
  (ccl:process-run-function "watchdog" #'watchdog)
  (dotimes (i *interrupters*)
    (push (ccl:process-run-function (format nil "interrupter-~d" i) #'interrupter) ints))
  (dotimes (i *max-live*) (ccl:signal-semaphore *slots*))
  (dotimes (i *threads*)
    (unless (ccl:timed-wait-on-semaphore *slots* (* 2 *stall-seconds*))
      (report nil "no thread exited for ~d s at thread ~d" (* 2 *stall-seconds*) i))
    (ccl:process-run-function
     (format nil "short-~d" i)
     (lambda ()
       ;; A little work, so that interrupts land at every stage of the
       ;; thread's life and not only at its start.
       (unwind-protect
            (let ((v (make-list (+ 1 (mod i 50)))))
              (ccl:with-lock-grabbed (*count-lock*)
                (setq v (length v))))
         (ccl:signal-semaphore *slots*))))
    (setq *progress* (1+ i)))
  (setq *done* t)
  (dolist (p ints)
    (ccl:process-wait-with-timeout
     "join" (* 30 ccl::*ticks-per-second*)
     (lambda () (ccl::process-exhausted-p p))))
  (report (plusp *sent*) "~d threads, ~d interrupts sent, ~d ran" *threads* *sent* *ran*))
