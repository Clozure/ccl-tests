;;;; extended-tests/backtrace/nvr-homed-local-recovery.lisp
;;;;
;;;; ANSI cannot reach this: it tests the debugger's recovery of a local
;;;; variable that the compiler homed in a callee-saved register.  The
;;;; defect it guards: populating *arm642-nvrs* (upstream 7c59f173) made the
;;;; arm64 compiler record register EAs in the symbol map, but
;;;; lib/arm64-backtrace.lisp still carried its empty-pool stubs --
;;;; REGISTERS-USED-BY answered (nil nil) and
;;;; %FIND-REGISTER-ARGUMENT-VALUE answered its BAD marker -- so every
;;;; NVR-homed local read as unavailable in backtrace, and the setter
;;;; path silently refused.  Fixed upstream by 104d1365, which added the
;;;; register-save trailer and the *saved-register-count* / names pair.
;;;; RED CONTROL: any arm64 image carrying the NVR pool but not that commit.
;;;;
;;;; Shape (same discipline as lexpr-frame-locals.lisp: frames are
;;;; observed from INSIDE a live extent, collect-only walks):
;;;;   VICTIM  fixnum accumulator C, loop-carried and live across a
;;;;           funcall => NVR-homed when the pool is on.
;;;;   GRABBER four competing fixnum locals => its frame claims and
;;;;           SAVES every pool register before the observation runs, so
;;;;           the %find-register-argument-value walk terminates there
;;;;           deterministically -- no exception frame needed.
;;;;   READ    FRAME-NAMED-VARIABLES on the victim's frame must show
;;;;           C = 384.
;;;;   SET     SET-MAP-ENTRY-VALUE on C must succeed and the victim,
;;;;           which returns c+1, must return the NEW value + 1: the
;;;;           write lands in the grabber's save cell and restore-nvrs
;;;;           carries it back into the register.  Getter and setter are
;;;;           tested together because they are a parity pair
;;;;           (%find/%set-register-argument-value).
;;;;
;;;; On the stock x86-64 ORACLE the same locals are vstack-homed
;;;; (*x8664-nvrs* is empty there), so the identical API assertions pass
;;;; through the raw-frame paths: the test asserts debugger BEHAVIOR the
;;;; standard never covers, not our register allocation.

(load (merge-pathnames "../harness.lisp" *load-pathname*))
(in-package :cl-user)

(defparameter *nvrbt-v* (make-array 64 :initial-element 2))

(defparameter *nvrbt-victim*
  (let ((ccl::*save-local-symbols* t))
    (compile nil
             '(lambda (n grab)
                (let ((v *nvrbt-v*) (c 0))
                  (declare (type simple-vector v) (fixnum c)
                           (optimize (speed 3) (safety 0)))
                  (dotimes (r n)
                    (declare (fixnum r))
                    (dotimes (i 64)
                      (declare (fixnum i))
                      (setq c (logand most-positive-fixnum
                                      (+ c (the fixnum (svref v i)))))))
                  (funcall grab)
                  (the fixnum (+ c 1)))))))

(defparameter *nvrbt-grab*
  (let ((ccl::*save-local-symbols* t))
    (compile nil
             '(lambda (out)
                (let ((v *nvrbt-v*) (a 0) (b 0) (d 0))
                  (declare (type simple-vector v) (fixnum a b d)
                           (optimize (speed 3) (safety 0)))
                  (dotimes (i 64)
                    (declare (fixnum i))
                    (setq a (logand most-positive-fixnum
                                    (+ a (the fixnum (svref v i))))
                          b (logand most-positive-fixnum (+ b a))
                          d (logand most-positive-fixnum (+ d b))))
                  (funcall out)
                  (logand most-positive-fixnum (+ a b d)))))))

(defmacro nvrbt-safely (form)
  `(handler-case ,form
     (serious-condition (c)
       (list :signalled (or (ignore-errors (princ-to-string c))
                            :unprintable)))))

(defun nvrbt-read-c ()
  "Value of the victim's C, read via FRAME-NAMED-VARIABLES from inside
the grabber's live extent."
  (let ((got :never-found))
    (ccl::map-call-frames
     (lambda (p context)
       (when (eq got :never-found)
         (multiple-value-bind (lfun pc) (ccl::cfp-lfun p)
           (declare (ignore pc))
           (when (eq lfun *nvrbt-victim*)
             (let* ((rows (ccl::frame-named-variables
                           p context :unknown-marker :unavailable))
                    (cell (and (listp rows)
                               (assoc "C" rows
                                      :key (lambda (k)
                                             (if (symbolp k)
                                                 (symbol-name k)
                                                 k))
                                      :test #'equal))))
               (setq got (if cell (cdr cell) :no-c-row))))))))
    got))

(defun nvrbt-set-c (newval)
  "Set the victim's C via SET-MAP-ENTRY-VALUE; non-nil on success."
  (let ((result :never-found))
    (ccl::map-call-frames
     (lambda (p context)
       (when (eq result :never-found)
         (multiple-value-bind (lfun pc) (ccl::cfp-lfun p)
           (when (eq lfun *nvrbt-victim*)
             (let* ((map (ccl::function-symbol-map lfun))
                    (names (car map)))
               (dotimes (i (length names))
                 (when (string= (symbol-name (svref names i)) "C")
                   (setq result
                         (ccl::set-map-entry-value context p lfun pc
                                                   i newval))))))))))
    result))

;;; READ: c = 3*64*2 = 384 at the observation point; victim returns 385.
(let* ((seen :not-run)
       (ret (nvrbt-safely
            (funcall *nvrbt-victim* 3
                     (lambda ()
                       (funcall *nvrbt-grab*
                                (lambda ()
                                  (setq seen (nvrbt-safely
                                              (nvrbt-read-c))))))))))
  (xt-check "nvr-local readable in backtrace" seen 384)
  (xt-check "victim undisturbed by the read" ret 385))

;;; SET: write 999 into C mid-flight; the victim must return 1000.
(let* ((set-ok :not-run)
       (ret (nvrbt-safely
            (funcall *nvrbt-victim* 3
                     (lambda ()
                       (funcall *nvrbt-grab*
                                (lambda ()
                                  (setq set-ok
                                        (nvrbt-safely
                                         (nvrbt-set-c 999))))))))))
  (xt-true "nvr-local settable in backtrace"
           (and set-ok (not (eq set-ok :not-run))
                (not (and (consp set-ok) (eq (car set-ok) :signalled)))))
  (xt-check "written value carried back into the frame" ret 1000))

(xt-report "NVR-SAVED-REGISTER-BACKTRACE")
