;;; TEST ONLY: arm64 runtime widener for the Lisp spin-word held window.
;;; %RELEASE-SPIN-LOCK is LAP on arm64, so the 0240 build-time widener
;;; (portable defun only) cannot reach it; this image also predates
;;; 0240, so the gating special is defined here.  Wrap the LAP release:
;;; hold the word for *SPIN-RELEASE-WIDEN-LOOPS* iterations, then call
;;; the original.  Non-consing.
(in-package :ccl)
(defparameter *spin-release-widen-loops* 0)
(let ((old (symbol-function '%release-spin-lock))
      (*warn-if-redefine-kernel* nil))
  (setf (symbol-function '%release-spin-lock)
        (lambda (p)
          (let ((n *spin-release-widen-loops*))
            (declare (fixnum n))
            (dotimes (i n)))
          (funcall old p))))
(format t "~&WIDEN-PRELUDE-LOADED~%")
