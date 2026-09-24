;;; Reproducer for the unbind_interrupt_level missed-suspend race
;;; (upstream issue #597, PR #634).  Test sleds:
;;;   ../wideners/sled-prefix-unbind-window.patch   (pre-fix behaviour)
;;;   ../wideners/sled-postfix-unbind-window.patch  (post-fix behaviour)
;;;
;;; The pre-fix subprimitive reads the pending-suspend flag BEFORE it
;;; restores *INTERRUPT-LEVEL*.  A suspend signal that lands between
;;; the read and the restore is deferred against the old level and the
;;; unbind returns without delivering it; the suspending thread then
;;; waits until the victim's NEXT unbind or exception heals the miss.
;;; So the observable is not a permanent wedge but an ACK-LATENCY
;;; SPIKE: the world-stop takes the remainder of the victim's work
;;; cycle instead of microseconds.
;;;
;;; Workers alternate a -2 bind/unbind (the window, held open by the
;;; sled) with a long non-consing spin (the latency the miss exposes).
;;; The stopper times every world-stop.  A stop over the threshold is
;;; a MISS; the run exits 42 when any miss occurred, 0 otherwise.
;;;
;;; Invocation (x86-64 host CCL, sled-patched scratch tree):
;;;   REPRO_ITERS=2000 <ccl> --no-init --batch --load this-file
;;; Result line: UNBIND-REPRO-RESULT :ITERS n :MISSES m :MAX-STOP-MS x

(in-package :cl-user)

(defvar *stop* nil)

(defvar *iters*
  (or (ignore-errors (parse-integer (ccl:getenv "REPRO_ITERS"))) 2000))

(defvar *nworkers*
  (or (ignore-errors (parse-integer (ccl:getenv "REPRO_WORKERS"))) 4))

;; Non-consing spin per worker cycle; the miss latency is up to this
;; long, so it must sit far above the normal stop time and above the
;; threshold below.  ~300M iterations is roughly 100-300 ms.
(defvar *worker-spin*
  (or (ignore-errors (parse-integer (ccl:getenv "REPRO_WORKER_SPIN")))
      300000000))

(defvar *miss-threshold-ms*
  (or (ignore-errors (parse-integer (ccl:getenv "REPRO_MISS_MS"))) 50))

(defvar *workers*
  (loop for i below *nworkers*
        collect (ccl:process-run-function
                 (format nil "unbind-worker-~d" i)
                 (lambda ()
                   (loop until *stop*
                         do (ccl::with-deferred-gc nil)
                            (let ((n *worker-spin*))
                              (declare (fixnum n))
                              (dotimes (i n))))))))

(format t "~&REPRO-START workers=~d iters=~d worker-spin=~d threshold=~dms~%"
        *nworkers* *iters* *worker-spin* *miss-threshold-ms*)
(force-output)

(let ((misses 0)
      (maxms 0))
  (dotimes (i *iters*)
    (let ((t0 (get-internal-real-time)))
      (ccl::with-other-threads-suspended nil)
      (let ((ms (round (* 1000 (- (get-internal-real-time) t0))
                       internal-time-units-per-second)))
        (when (> ms maxms) (setq maxms ms))
        (when (>= ms *miss-threshold-ms*)
          (incf misses)
          (format t "~&MISS ~dms at iteration ~d~%" ms i)
          (force-output))))
    (when (zerop (mod i 100))
      (format t "~&HEARTBEAT ~d max=~dms misses=~d~%" i maxms misses)
      (force-output)))
  (setq *stop* t)
  (sleep 0.5)
  (format t "~&UNBIND-REPRO-RESULT :ITERS ~d :MISSES ~d :MAX-STOP-MS ~d~%"
          *iters* misses maxms)
  (format t "~&REPRO-COMPLETE~%")
  (force-output)
  (ccl:quit (if (> misses 0) 42 0)))
