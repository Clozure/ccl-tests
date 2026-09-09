;;; Reproducer for the suspend-vs-spinlock runtime deadlock.
;;;
;;; A thread that is suspended by a world-stop while it holds a lock
;;; guard spin word never releases it.  The one world-stop that returns
;;; with the world still stopped (%SUSPEND-OTHER-THREADS) must then take
;;; the exception lock's guard spin word to RELEASE the exception lock,
;;; and it spins forever.  Workers cons continuously so allocation traps
;;; keep passing through the exception lock's guard spin word; the
;;; initial thread repeatedly stops and resumes the world, exactly as a
;;; periodic heap-utilization probe does.
;;;
;;; Driver: run-suspend-spinlock.sh in this directory.  Environment:
;;;   REPRO_ITERS    world-stop iterations (default 3000)
;;;   REPRO_WORKERS  consing worker threads (default 4)
;;;   CCL_SPINLOCK_WIDEN_LOOPS  honored by the TEST-ONLY widener patch
;;;     in the kernel; without that patch it has no effect.
;;;
;;; Output contract (the driver greps for these):
;;;   REPRO-START ...     printed before the first world-stop
;;;   HEARTBEAT <n>       printed every 100 world-stops
;;;   REPRO-COMPLETE      printed only when every iteration finished

(in-package :cl-user)

(defvar *stop* nil)

(defvar *iters*
  (or (ignore-errors (parse-integer (ccl:getenv "REPRO_ITERS"))) 3000))

(defvar *nworkers*
  (or (ignore-errors (parse-integer (ccl:getenv "REPRO_WORKERS"))) 4))

(defvar *workers*
  (loop for i below *nworkers*
        collect (ccl:process-run-function
                 (format nil "consing-worker-~d" i)
                 (lambda ()
                   (let ((sink nil))
                     (declare (ignorable sink))
                     (loop until *stop*
                           do (setq sink (make-list 256))))))))

(format t "~&REPRO-START workers=~d iters=~d widen=~a~%"
        *nworkers* *iters*
        (or (ccl:getenv "CCL_SPINLOCK_WIDEN_LOOPS") "0"))
(force-output)

(dotimes (i *iters*)
  (ccl::with-other-threads-suspended
    ;; A tiny body, like the world-stop a heap-utilization probe takes.
    nil)
  (when (zerop (mod i 100))
    (format t "~&HEARTBEAT ~d~%" i)
    (force-output)))

(setq *stop* t)
(sleep 0.2)
(format t "~&REPRO-COMPLETE~%")
(force-output)
(ccl:quit 0)
