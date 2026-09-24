;;;; tools/beyond-ansi/lexpr-backtrace-map-test.lisp — Clozure/ccl#600 guard.
;;;;
;;;; ANSI cannot reach this: it tests the debugger's symbol-map decode of
;;;; &lexpr (CCL-only) function frames.  The defect it guards: the arm64
;;;; lexpr prologue recorded the frame's savevsp BELOW the symbol map's
;;;; vloc-0 origin by 8*num-fixed bytes, so every map entry of a lexpr
;;;; function read one slot below its variable.  SUPPLIED-ARGUMENT-LIST
;;;; then dereferenced a misread fixnum as a lexpr pointer and faulted at
;;;; machine address 8 -- the "(/ 0 0) then :b" crash Gary Palter reported.
;;;;
;;;; Fixed upstream by 2ccb2a20, which builds the lexpr frame BEFORE the
;;;; fixed-arg copies.  RED CONTROL: any arm64 image predating that commit; on it these cases fail with a fault-during-read signal or with
;;;; off-by-slot values.  All cases pass on stock x86-64 CCL.
;;;;
;;;; TEST DISCIPLINE, both learned on the oracle 2026-08-14:
;;;;   * Guards are SERIOUS-CONDITION, not ERROR: the red-image failure is
;;;;     CCL::INVALID-MEMORY-ACCESS, a STORAGE-CONDITION
;;;;     defined in l1-error-system.lisp, so an ERROR handler lets it reach the
;;;;     break loop and kill the run.
;;;;   * Frames are observed FROM INSIDE A LIVE HANDLER, the same way the
;;;;     break loop does.  On x8664 a frame captured mid-walk names a call
;;;;     site; after the finder returns, that frame is stale and decoding
;;;;     it walks into funcall-machinery words ("... is not a valid stack
;;;;     frame" from %FRAME-BACKLINK).  CCL::ARGUMENT-VALUE is avoided for
;;;;     the same reason -- the live paths are ARGUMENTS-AND-LOCALS /
;;;;     MAP-ENTRY-VALUE / SUPPLIED-ARGUMENT-LIST.
;;;;
;;;; Two shapes:
;;;;   * the literal issue-600 repro (`/', num-fixed = 1);
;;;;   * a user &lexpr function with num-fixed = 2, so the shift is two
;;;;     slots and cannot pass by luck of adjacent equal values.

(load (merge-pathnames "../harness.lisp" *load-pathname*))
(in-package :cl-user)

(defmacro bt600-safely (form)
  "Normalize any signal (including storage-conditions) to a value."
  `(handler-case ,form
     (serious-condition (c)
       (list :signalled (or (ignore-errors (princ-to-string c))
                            :unprintable)))))

;;; Find the live frame attributed to LFUN.  Collect-only full walk via
;;; MAP-CALL-FRAMES (a non-local exit out of the callback perturbs the
;;; walk); first hit in walk order is the frame nearest the signal.
(defun bt600-find-frame (lfun)
  (let ((hits nil))
    (ccl::map-call-frames
     (lambda (p context)
       (declare (ignore context))
       (multiple-value-bind (fn pc) (ccl::cfp-lfun p)
         (when (eq fn lfun)
           (push (cons p pc) hits)))))
    (let ((hit (car (last hits))))
      (values (car hit) (cdr hit)))))

(defun bt600-map-value (frame lfun pc name)
  "The raw map-entry-value for NAME in LFUN's symbol map."
  (let* ((map (ccl::function-symbol-map lfun))
         (names (car map)))
    (dotimes (i (length names) :not-in-map)
      (when (eq (svref names i) name)
        (return (ccl::map-entry-value nil frame lfun pc i :unavailable))))))

;;; --- Shape 1: the issue-600 repro ------------------------------------

(defun bt600-slash-args ()
  "Signal (/ 0 0); from the handler, reconstruct /'s supplied arguments."
  (catch :bt600
    (handler-bind
        ((error
          (lambda (c)
            (declare (ignore c))
            (multiple-value-bind (f pc)
                (bt600-find-frame (symbol-function '/))
              (throw :bt600
                (if (null f)
                    :no-frame
                    (bt600-safely
                     (multiple-value-bind (args valid)
                         (ccl::supplied-argument-list
                          nil f (symbol-function '/) pc)
                       (list valid args)))))))))
      (let ((a 0) (b 0))
        (declare (notinline /))
        (/ a b)))))

(xt-check "bt600.slash-supplied-args" (bt600-slash-args) '(t (0 0))
          :test #'equal)

;;; --- Shape 2: user &lexpr, num-fixed = 2 ------------------------------
;;;
;;; TWO MORE red-image hazards, both bounded here:
;;;   * A misread MORE slot can hold a VALID stack address whose word
;;;     decodes as an astronomically large count -- WITH-LIST-FROM-LEXPR
;;;     then loops for billions of iterations (watched hang the first
;;;     red run of this shape).  So SUPPLIED-ARGUMENT-LIST runs only
;;;     after the count word passes a sanity precheck.
;;;   * A misread slot can hold structure that unbounded ~S printing
;;;     never finishes.  All compared values go through BT600-SANE
;;;     (print-circle + level/length bounds) before BA-CHECK sees them.

(defun bt600-sane (v)
  "Bounded, cycle-safe rendering of V for comparison and reporting."
  (let ((*print-circle* t) (*print-level* 4) (*print-length* 12)
        (*print-readably* nil))
    (or (ignore-errors (format nil "~s" v)) "<unprintable>")))

(defun bt600-victim (a b ccl::&lexpr more)
  "Signals at a known point; the handler observes this frame while live."
  (declare (ignorable a b more))
  (error "bt600 probe point"))

(defun bt600-run-victim ()
  (catch :bt600
    (handler-bind
        ((simple-error
          (lambda (c)
            (declare (ignore c))
            (let ((lfun (symbol-function 'bt600-victim)))
              (multiple-value-bind (f pc) (bt600-find-frame lfun)
                (throw :bt600
                  (if (null f)
                      :no-frame
                      (let* ((mv (bt600-safely (bt600-map-value f lfun pc 'more)))
                             (cnt (and (typep mv 'fixnum) (> mv 4096)
                                       (bt600-safely
                                        (ccl::%fixnum-ref-natural mv 0)))))
                        (list
                         (bt600-safely (bt600-map-value f lfun pc 'a))
                         (bt600-safely (bt600-map-value f lfun pc 'b))
                         ;; Only reconstruct the arg list when the count
                         ;; word is sane; a garbage count loops forever.
                         (if (and (typep cnt 'integer) (<= 0 cnt 512)
                                  (zerop (mod cnt 8)))
                             (bt600-safely
                              (multiple-value-bind (args valid)
                                  (ccl::supplied-argument-list nil f lfun pc)
                                (list valid args)))
                             (list :lexpr-precheck-failed mv cnt))
                         (if cnt cnt (list :not-a-pointer mv)))))))))))
      (bt600-victim 10 20 30 40 50))))

(let ((r (bt600-safely (bt600-run-victim))))
  (flet ((elt-sane (n) (bt600-sane (and (consp r) (nth n r)))))
    (xt-check "bt600.victim-a"    (elt-sane 0) (bt600-sane 10) :test #'string=)
    (xt-check "bt600.victim-b"    (elt-sane 1) (bt600-sane 20) :test #'string=)
    (xt-check "bt600.victim-args" (elt-sane 2)
              (bt600-sane '(t (10 20 30 40 50))) :test #'string=)
    ;; MORE's map value must be a real lexpr pointer: its count word is
    ;; the boxed number of non-fixed args, (5 - 2) << 3 = 24.
    (xt-check "bt600.victim-more-count" (elt-sane 3) (bt600-sane 24)
              :test #'string=)))

(xt-report "LEXPR-BACKTRACE-MAP")
