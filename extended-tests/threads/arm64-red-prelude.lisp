;;; TEST ONLY: neuter the 0236 deferral at run time, simulating the
;;; unfixed image: %GET-SPIN-LOCK-DEFERRING-SUSPENSION becomes a plain
;;; spin-acquire with no interrupt-level manipulation, so a world-stop
;;; can park a thread while it holds the spin word.  The sites still
;;; call this function; only the protection is removed.
(in-package :ccl)
(let ((*warn-if-redefine-kernel* nil))
(defun %get-spin-lock-deferring-suspension (p)
  (let* ((self (%current-tcr))
         (n *spin-lock-tries*))
    (declare (fixnum n))
    (loop
      (dotimes (i n)
        (when (eql 0 (%ptr-store-fixnum-conditional p 0 self))
          (return-from %get-spin-lock-deferring-suspension t)))
      (yield)))))
(format t "~&RED-PRELUDE-LOADED (0236 deferral neutered)~%")
