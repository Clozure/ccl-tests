;;; RED/GREEN control for the recursive_lock_trylock count leak
;;; (upstream issue #597, PR #634).
;;;
;;; The defect: in the already-owned arm with was_free NULL, the count
;;; is raised and the function then falls through to return EBUSY.
;;; The caller, told it did not acquire, releases once for its one
;;; acquisition -- and the lock stays owned forever.
;;;
;;; No production caller exists; the function is reachable through the
;;; kernel-import vector, the same idiom l0-aprims.lisp uses for
;;; new-recursive-lock.  The TCR argument is the CALLING thread's,
;;; converted exactly as level-0 does, so the owner comparison is real.
;;;
;;; The observable is EXTERNAL ownership, not an internal field:
;;;   1. this thread grab-locks once
;;;   2. raw ff-call recursive_lock_trylock, was_free = NULL
;;;   3. release once, plus once more ONLY IF the raw call reported 0
;;;   4. a second Lisp process try-locks
;;; Defective: raw returns EBUSY having secretly raised the count, so
;;; one release leaves the lock owned and the second process fails.
;;; Fixed: raw returns 0, both releases land, the second process
;;; acquires.  After a RED observation the leaked count is released so
;;; the image is not left wedged.
;;;
;;; Contract:  TRYLOCK-RESULT :RAW-RC r :SECOND-ACQUIRED b :VERDICT v
;;; Exit 0 on PASS, 42 on FAIL.

(in-package :cl-user)

(defvar *lock* (ccl:make-lock))          ; recursive lock via l0-aprims
(defvar *probe-result* :unset)
(defvar *probe-done* (ccl:make-semaphore))

(let* ((ptr (ccl::recursive-lock-ptr *lock*)))
  (ccl:grab-lock *lock*)                 ; own it once
  (ccl::with-macptrs ((self))
    (ccl::%setf-macptr-to-object self (ccl::%current-tcr))
    (let ((raw-rc (ccl:ff-call
                   (ccl::%kernel-import
                    target::kernel-import-recursive-lock-trylock)
                   :address ptr
                   :address self
                   :address (ccl::%null-ptr)
                   :signed-fullword)))
      ;; the return-code contract: 0 = acquired
      (ccl:release-lock *lock*)
      (when (eql raw-rc 0)
        (ccl:release-lock *lock*))
      ;; external observer: can another thread take the lock now?
      (ccl:process-run-function "trylock-prober"
        (lambda ()
          (setq *probe-result* (ccl::try-lock *lock*))
          (when *probe-result*
            (ccl:release-lock *lock*))
          (ccl:signal-semaphore *probe-done*)))
      (let* ((joined (ccl:timed-wait-on-semaphore *probe-done* 20))
             (acquired (and joined (eq *probe-result* t)))
             (verdict (if acquired "PASS" "FAIL")))
        ;; clean up a RED leak so the image is usable afterwards
        (unless acquired
          (ignore-errors (ccl:release-lock *lock*)))
        (format t "~&TRYLOCK-RESULT :RAW-RC ~d :SECOND-ACQUIRED ~a :VERDICT ~a~%"
                raw-rc (and acquired t) verdict)
        (force-output)
        (ccl:quit (if acquired 0 42))))))
