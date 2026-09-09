;;; Lisp-side reproducer for the suspend-vs-spin-word deadlock
;;; (upstream issue #597, PR #634).
;;; Test widener: ../wideners/widen-lisp-spin-release.patch
;;;
;;; Vehicle: rwlock READ locks only.  Reader ownership never blocks
;;; another reader, so a suspended reader that OWNS the lock cannot
;;; stall the world-stopping thread; the only resource that can wedge
;;; it is the lock's guard spin word.  A wedge here is therefore the
;;; spin-word defect and not the by-design "suspended lock owner"
;;; hazard.  Workers hammer read-lock/unlock; the initial thread
;;; repeatedly stops the world and takes a read lock inside the stop.
;;;
;;; The test widener is armed at run time through
;;; CCL::*SPIN-RELEASE-WIDEN-LOOPS* when REPRO_LISP_WIDEN is set and
;;; the build carries the widener; on a build without it the variable
;;; is absent and this file still runs (unwidened).
;;;
;;; Output contract (driver: run-suspend-spinlock.sh with
;;; REPRO_FILE pointing here): REPRO-START / HEARTBEAT n / REPRO-COMPLETE.

(in-package :cl-user)

(defvar *stop* nil)

(defvar *iters*
  (or (ignore-errors (parse-integer (ccl:getenv "REPRO_ITERS"))) 3000))

(defvar *nworkers*
  (or (ignore-errors (parse-integer (ccl:getenv "REPRO_WORKERS"))) 4))

(defvar *rw* (ccl::make-read-write-lock))

(defvar *lisp-widen*
  (let ((n (or (ignore-errors
                 (parse-integer (or (ccl:getenv "REPRO_LISP_WIDEN") "0")))
               0))
        (sym (find-symbol "*SPIN-RELEASE-WIDEN-LOOPS*" :ccl)))
    (if (and sym (boundp sym))
      (progn (when (> n 0) (set sym n)) n)
      :absent)))

(defvar *workers*
  (loop for i below *nworkers*
        collect (ccl:process-run-function
                 (format nil "rw-reader-~d" i)
                 (lambda ()
                   (loop until *stop*
                         do (ccl::read-lock-rwlock *rw*)
                            (ccl::unlock-rwlock *rw*))))))

(format t "~&REPRO-START workers=~d iters=~d lisp-widen=~a~%"
        *nworkers* *iters* *lisp-widen*)
(force-output)

(dotimes (i *iters*)
  (ccl::with-other-threads-suspended
    (ccl::read-lock-rwlock *rw*)
    (ccl::unlock-rwlock *rw*))
  (when (zerop (mod i 100))
    (format t "~&HEARTBEAT ~d~%" i)
    (force-output)))

(setq *stop* t)
(sleep 0.2)
(format t "~&REPRO-COMPLETE~%")
(force-output)
(ccl:quit 0)
