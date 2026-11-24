;;;-*-Mode: LISP; Package: CL-TEST -*-
;;;
;;; CCL-specific tests.  New tests go here, or in compiler.lisp for
;;; compiler tests; the older ones are still in ansi-tests/ccl.lsp,
;;; which is loaded first, so its helpers (such as TEST-COMPILE and
;;; TEST-SOURCE-FILE) are available here.
;;;
;;; The tests run with the current directory set to ansi-tests/, so a
;;; relative file name in a test names a file there.

(in-package :cl-test)

(deftest ccl.issue#544.stassats
    (values (array-element-type (make-array 0 :element-type 'base-char))
            (upgraded-array-element-type 'base-char))
  character
  character)

;;; UPGRADED-ARRAY-ELEMENT-TYPE and ARRAY-ELEMENT-TYPE got their answers
;;; from separate tables, which disagreed about strings (BASE-CHAR vs
;;; CHARACTER).  They should name the same type specifier.
(deftest ccl.issue#544
    (loop for type in '(base-char character standard-char bit (unsigned-byte 8)
                        (signed-byte 8) (unsigned-byte 16) (signed-byte 16)
                        (unsigned-byte 32) (signed-byte 32) fixnum
                        #+64-bit-target (unsigned-byte 64)
                        #+64-bit-target (signed-byte 64)
                        single-float double-float
                        (complex single-float) (complex double-float) t)
          unless (equal (array-element-type (make-array 0 :element-type type))
                        (upgraded-array-element-type type))
            collect type)
  nil)

;;; TRUNCATE by a ratio whose numerator is 1 returned a temporary bignum
;;; from the stack, so FLOOR, CEILING, ROUND, MOD and REM gave a bogus
;;; object or a wrong value.
(deftest ccl.issue#733
    (let* ((tiny (/ (expt 2 70)))
           (q (* 3 (expt 2 68))))
      (loop for (name got want)
              in (list (list 'truncate
                             (multiple-value-list (truncate 3/4 tiny))
                             (list q 0))
                       (list 'floor
                             (multiple-value-list (floor 3/4 tiny))
                             (list q 0))
                       (list 'ceiling
                             (multiple-value-list (ceiling 3/4 tiny))
                             (list q 0))
                       (list 'round
                             (multiple-value-list (round 3/4 tiny))
                             (list q 0))
                       (list 'mod (mod 3/4 tiny) 0)
                       (list 'rem (rem 3/4 tiny) 0))
            unless (equal got want)
              collect name))
  nil)

;;; The same bug, for a bignum dividend (or MOST-NEGATIVE-FIXNUM, which is
;;; handled as one).  A dangling quotient still has the right value until
;;; something reuses its stack, so do every division before checking any
;;; of them: each one lands where the previous one's temporary was.
(deftest ccl.issue#733.bignum
    (let* ((cases (list (list (expt 2 100) 10)
                        (list (- (expt 3 70)) 7)
                        (list (1+ (expt 2 64)) 3)
                        (list most-negative-fixnum (expt 2 70))
                        (list (- (expt 5 60)) 1000)
                        (list (expt 11 40) 13)))
           (quotients (loop for (x d) in cases
                            collect (truncate x (/ d)))))
      (loop for (x d) in cases
            for got in quotients
            unless (eql got (* x d))
              collect x))
  nil)

;;; APPLY-IN-FRAME and RETURN-FROM-FRAME, and the break loop's
;;; :apply-in-frame and :return-from-frame, signaled "%apply-in-frame has
;;; bitrotted" on x86-64.  These tests do what the break loop does without
;;; one: a handler for ERROR finds a frame with MAP-CALL-FRAMES and calls
;;; the function under test from there, in the thread that signaled.  A
;;; frame exists for a target because ERROR is called, not jumped to, from
;;; the function that signals, and the functions that call those use VALUES
;;; so they are not tail calls; CALL-F-FROM-G tail-calls and has no frame,
;;; and no test targets it.  Each test looks its target up by function, so
;;; a missing frame fails the test instead of returning from another one.
#+x8664-target
(progn
  (declaim (notinline signal-in-f call-f-from-g bind-and-signal
                      call-bind-and-signal signal-under-cleanup
                      call-signal-under-cleanup catch-and-signal
                      call-catch-and-signal fail-once car-of call-car-of))

  (defvar *inner-binding* :global)
  (defvar *outer-binding* :global)
  (defvar *fail-once-calls* 0)
  (defvar *cleanup-count* 0)

  ;; The newest frame of the function called NAME that is not an exception
  ;; frame, which carries the function of the code that trapped.
  (defun frame-of-function (name)
    (let ((function (fdefinition name))
          (found nil))
      (ccl:map-call-frames
       (lambda (p context)
         (declare (ignore context))
         (when (and (null found)
                    (not (ccl::xcf-p p))
                    (eq (ccl::cfp-lfun p) function))
           (setq found p))))
      found))

  (defun call-with-error-handler (thunk handler)
    (handler-bind ((error (lambda (c)
                            (declare (ignore c))
                            (funcall handler))))
      (funcall thunk)))

  (defun signal-in-f ()
    (error "x"))

  (defun call-f-from-g ()
    (signal-in-f))

  (defun bind-and-signal ()
    (let ((*inner-binding* :bound))
      (error "x")))

  (defun call-bind-and-signal ()
    (values (bind-and-signal)))

  (defun signal-under-cleanup ()
    (unwind-protect (error "x")
      (incf *cleanup-count*)))

  (defun call-signal-under-cleanup ()
    (values (signal-under-cleanup)))

  (defun catch-and-signal ()
    (catch 'catch-and-signal-tag
      (error "x")))

  (defun call-catch-and-signal ()
    (values (catch-and-signal)))

  (defun fail-once ()
    (if (eql (incf *fail-once-calls*) 1)
        (error "x")
        :second))

  (defun car-of (x)
    (car x))

  (defun call-car-of (x)
    (values (car-of x)))

  (defun tcr-xframe-and-foreign-sp ()
    (let ((tcr (ccl::%current-tcr)))
      (list (ccl::%fixnum-ref tcr (- target::tcr.xframe target::tcr-bias))
            (ccl::%fixnum-ref tcr (- target::tcr.foreign-sp target::tcr-bias)))))

  ;; The newest frame whose parent is an exception frame.
  (defun frame-below-xcf ()
    (let ((newer nil)
          (found nil))
      (ccl:map-call-frames
       (lambda (p context)
         (declare (ignore context))
         (when (and (null found) (ccl::xcf-p p))
           (setq found newer))
         (setq newer p)))
      found))

  ;; Return 42 from the frame of SIGNAL-IN-F, which CALL-F-FROM-G returns.
  (deftest ccl.issue#497
      (call-with-error-handler
       #'call-f-from-g
       (lambda ()
         (ccl::return-from-frame (frame-of-function 'signal-in-f) 42)))
    42)

  ;; Return two values the same way; the caller sees both.
  (deftest ccl.issue#497.values
      (multiple-value-list
       (call-with-error-handler
        #'call-f-from-g
        (lambda ()
          (ccl::return-from-frame (frame-of-function 'signal-in-f) 1 2))))
    (1 2))

  ;; The function in the frame is called again, with the frame as its
  ;; caller: the counter goes from 1 to 2 and the second call's value is
  ;; what the frame returns.
  (deftest ccl.issue#497.reinvoke
      (let ((calls-in-handler nil))
        (setq *fail-once-calls* 0)
        (let ((value (call-with-error-handler
                      #'fail-once
                      (lambda ()
                        (setq calls-in-handler *fail-once-calls*)
                        (ccl:apply-in-frame (frame-of-function 'fail-once)
                                            'fail-once nil)))))
          (list calls-in-handler *fail-once-calls* value)))
    (1 2 :second))

  ;; BIND-AND-SIGNAL binds *INNER-BINDING* between the target and the error.
  ;; That binding is undone, and the one made outside the target is not.
  (deftest ccl.issue#497.special
      (let ((*outer-binding* :outer)
            (inner-in-handler nil))
        (let ((value (call-with-error-handler
                      #'call-bind-and-signal
                      (lambda ()
                        (setq inner-in-handler *inner-binding*)
                        (ccl::return-from-frame
                         (frame-of-function 'call-bind-and-signal) 42)))))
          (list value inner-in-handler *inner-binding* *outer-binding*)))
    (42 :bound :global :outer))

  ;; The cleanup form of an UNWIND-PROTECT between the target and the error
  ;; runs once.
  (deftest ccl.issue#497.unwind-protect
      (progn
        (setq *cleanup-count* 0)
        (let ((value (call-with-error-handler
                      #'call-signal-under-cleanup
                      (lambda ()
                        (ccl::return-from-frame
                         (frame-of-function 'call-signal-under-cleanup) 42)))))
          (list value *cleanup-count*)))
    (42 1))

  ;; A CATCH between the target and the error, with nothing bound in between:
  ;; the throw to it leaves the binding stack where the target frame had it,
  ;; so there is nothing left to unbind.
  (deftest ccl.issue#497.catch
      (call-with-error-handler
       #'call-catch-and-signal
       (lambda ()
         (ccl::return-from-frame
          (frame-of-function 'call-catch-and-signal) 42)))
    42)

  ;; (car 1) traps into the kernel, which pushes an exception frame that
  ;; stands for the frame of CAR-OF.  The target is CALL-CAR-OF, the frame
  ;; above it.
  (deftest ccl.issue#497.trap
      (let* ((before (tcr-xframe-and-foreign-sp))
             (value (call-with-error-handler
                     (lambda () (call-car-of 1))
                     (lambda ()
                       (ccl::return-from-frame (frame-of-function 'call-car-of)
                                               42))))
             (after (tcr-xframe-and-foreign-sp)))
        (list value (equal before after)))
    (42 t))

  ;; When the parent of the target is the exception frame, there is nothing
  ;; to return to, and APPLY-IN-FRAME says so before it unwinds anything.
  (deftest ccl.issue#497.xcf-parent
      (let ((message (catch 'xcf-parent
                       (call-with-error-handler
                        (lambda ()
                          (call-car-of 1))
                        (lambda ()
                          (handler-case
                              (ccl::return-from-frame (frame-below-xcf) 42)
                            (error (c)
                              (throw 'xcf-parent (princ-to-string c)))))))))
        (and (stringp message)
             (eql 0 (search "Can't unwind to exception frame" message))))
    t))

;;; A function that keeps a float or a 64-bit integer across a call keeps
;;; it in an NFP frame on the foreign stack, found through tcr.nfp.
;;; APPLY-IN-FRAME left tcr.nfp and tcr.foreign-sp pointing at the frame
;;; of the function it had left, and that function's caller then read its
;;; own floats from there: ((42 15.0D0) NIL) for the first test.  Each
;;; test leaves functions that keep floats, checks what the caller makes
;;; of its own float afterwards, and compares tcr.nfp, tcr.foreign-sp and
;;; tcr.xframe before and after.
#+x8664-target
(progn
  (declaim (notinline keep-float-around keep-float-and-call
                      keep-float-and-signal keep-float-and-trap
                      catch-around-floats cleanup-around-floats
                      stack-block-around stack-block-and-call
                      keep-eight-floats-around keep-float-and-vector-around
                      call-out-and-signal))

  (defvar *float-cleanup-count* 0)

  (defun tcr-nfp-foreign-sp-and-xframe ()
    (let ((tcr (ccl::%current-tcr)))
      (list (ccl::%fixnum-ref tcr (- target::tcr.nfp target::tcr-bias))
            (ccl::%fixnum-ref tcr (- target::tcr.foreign-sp target::tcr-bias))
            (ccl::%fixnum-ref tcr (- target::tcr.xframe target::tcr-bias)))))

  ;; What CALL-WITH-ERROR-HANDLER returns, and whether the three words of
  ;; the TCR are what they were before it.
  (defun value-and-tcr-kept (thunk handler)
    (let* ((before (tcr-nfp-foreign-sp-and-xframe))
           (value (call-with-error-handler thunk handler))
           (after (tcr-nfp-foreign-sp-and-xframe)))
      (list value (equal before after))))

  ;; Keeps Y across the call of THUNK.
  (defun keep-float-around (x thunk)
    (declare (double-float x) (optimize (speed 3) (safety 0)))
    (let ((y (* x 3d0)))
      (values (funcall thunk))
      (+ y 1d0)))

  ;; Keeps D across the call of KEEP-FLOAT-AROUND, and returns what that
  ;; returned with D.
  (defun keep-float-and-call (x thunk)
    (declare (double-float x) (optimize (speed 3) (safety 0)))
    (let ((d (* x 2d0)))
      (let ((r (keep-float-around (+ x 4d0) thunk)))
        (list r (+ d 0d0)))))

  (defun keep-float-and-signal (x)
    (declare (double-float x) (optimize (speed 3) (safety 0)))
    (let ((z (* x 5d0)))
      (values (signal-in-f))
      (+ z 1d0)))

  (defun keep-float-and-trap (x)
    (declare (double-float x) (optimize (speed 3) (safety 0)))
    (let ((z (* x 5d0)))
      (values (car-of 1))
      (+ z 1d0)))

  (defun catch-around-floats (x)
    (declare (double-float x) (optimize (speed 3) (safety 0)))
    (let ((w (* x 7d0)))
      (catch 'catch-around-floats-tag
        (keep-float-and-signal x))
      (+ w 1d0)))

  (defun cleanup-around-floats (x)
    (declare (double-float x) (optimize (speed 3) (safety 0)))
    (let ((w (* x 7d0)))
      (unwind-protect
           (keep-float-and-signal x)
        (keep-float-around x (lambda () (incf *float-cleanup-count*))))
      (+ w 1d0)))

  (defun stack-block-around (x thunk)
    (declare (double-float x) (optimize (speed 3) (safety 0)))
    (let ((y (* x 3d0)))
      (ccl:%stack-block ((p 64))
        (setf (ccl:%get-double-float p 8) y)
        (values (funcall thunk)))
      (+ y 1d0)))

  (defun stack-block-and-call (x thunk)
    (declare (double-float x) (optimize (speed 3) (safety 0)))
    (let ((d (* x 2d0)))
      (ccl:%stack-block ((q 32))
        (setf (ccl:%get-double-float q 8) 9d0)
        (let ((r (stack-block-around (+ x 4d0) thunk)))
          (list r (+ d 0d0) (ccl:%get-double-float q 8))))))

  ;; Eight floats kept across the call: an NFP frame of more than 127
  ;; bytes.
  (defun keep-eight-floats-around (x thunk)
    (declare (double-float x) (optimize (speed 3) (safety 0)))
    (let ((a (+ x 1d0)) (b (+ x 2d0)) (c (+ x 3d0)) (d (+ x 4d0))
          (e (+ x 5d0)) (f (+ x 6d0)) (g (+ x 7d0)) (h (+ x 8d0)))
      (values (funcall thunk))
      (list a b c d e f g h)))

  ;; Keeps Y and V across the call of THUNK.  V is too large for the
  ;; stack: the kernel allocates it in the heap and makes a frame of two
  ;; words on the foreign stack in its place.
  (defun keep-float-and-vector-around (x thunk)
    (declare (double-float x) (optimize (speed 3) (safety 0)))
    (let ((y (* x 3d0))
          (v (make-array 100000 :element-type '(unsigned-byte 8))))
      (declare (dynamic-extent v))
      (setf (aref v 0) 1)
      (values (funcall thunk))
      (+ y (aref v 0))))

  ;; Return 42 from KEEP-FLOAT-AROUND; its caller adds its own float.
  (deftest ccl.issue#497.unboxed
      (value-and-tcr-kept
       (lambda () (keep-float-and-call 1d0 #'signal-in-f))
       (lambda ()
         (ccl::return-from-frame (frame-of-function 'keep-float-around) 42)))
    ((42 2.0d0) t))

  ;; Return from the oldest function that keeps a float.
  (deftest ccl.issue#497.unboxed-outer
      (value-and-tcr-kept
       (lambda () (keep-float-and-call 1d0 #'signal-in-f))
       (lambda ()
         (ccl::return-from-frame (frame-of-function 'keep-float-and-call) 42)))
    (42 t))

  ;; The error is a trap, so an exception frame lies between.
  (deftest ccl.issue#497.unboxed-trap
      (value-and-tcr-kept
       (lambda () (keep-float-and-call 1d0 (lambda () (car-of 1))))
       (lambda ()
         (ccl::return-from-frame (frame-of-function 'keep-float-around) 42)))
    ((42 2.0d0) t))

  ;; A CATCH lies between, with a float kept on each side of it.
  (deftest ccl.issue#497.unboxed-catch
      (value-and-tcr-kept
       (lambda ()
         (keep-float-and-call 1d0 (lambda () (catch-around-floats 1d0))))
       (lambda ()
         (ccl::return-from-frame (frame-of-function 'keep-float-around) 42)))
    ((42 2.0d0) t))

  ;; An UNWIND-PROTECT lies between; its cleanup keeps a float and runs
  ;; once.
  (deftest ccl.issue#497.unboxed-cleanup
      (progn
        (setq *float-cleanup-count* 0)
        (list (value-and-tcr-kept
               (lambda ()
                 (keep-float-and-call 1d0
                                      (lambda () (cleanup-around-floats 1d0))))
               (lambda ()
                 (ccl::return-from-frame (frame-of-function 'keep-float-around)
                                         42)))
              *float-cleanup-count*))
    (((42 2.0d0) t) 1))

  ;; The function applied in the frame keeps a float itself.
  (deftest ccl.issue#497.unboxed-reinvoke
      (value-and-tcr-kept
       (lambda () (keep-float-and-call 1d0 #'signal-in-f))
       (lambda ()
         (ccl:apply-in-frame (frame-of-function 'keep-float-around)
                             'keep-float-around
                             (list 6d0 (lambda () nil)))))
    ((19.0d0 2.0d0) t))

  ;; The target and its caller each have a stack block as well as a float.
  (deftest ccl.issue#497.stack-block
      (value-and-tcr-kept
       (lambda () (stack-block-and-call 1d0 #'signal-in-f))
       (lambda ()
         (ccl::return-from-frame (frame-of-function 'stack-block-around) 42)))
    ((42 2.0d0 9.0d0) t))

  ;; The handler of a trap traps itself, under a function that keeps a
  ;; float: two exception frames lie between.
  (deftest ccl.issue#497.unboxed-nested
      (value-and-tcr-kept
       (lambda () (keep-float-and-call 1d0 (lambda () (car-of 1))))
       (lambda ()
         (call-with-error-handler
          (lambda () (keep-float-and-trap 1d0))
          (lambda ()
            (ccl::return-from-frame (frame-of-function 'keep-float-around)
                                    42)))))
    ((42 2.0d0) t))

  ;; A function with a large NFP frame returns its floats, and is left.
  (deftest ccl.issue#497.unboxed-many
      (list (keep-eight-floats-around 0d0 (lambda () nil))
            (value-and-tcr-kept
             (lambda ()
               (keep-float-and-call
                1d0
                (lambda () (keep-eight-floats-around 0d0 #'signal-in-f))))
             (lambda ()
               (ccl::return-from-frame (frame-of-function 'keep-float-around)
                                       42))))
    ((1.0d0 2.0d0 3.0d0 4.0d0 5.0d0 6.0d0 7.0d0 8.0d0) ((42 2.0d0) t)))

  ;; A function with a dynamic-extent vector too large for the stack lies
  ;; between.
  (deftest ccl.issue#497.unboxed-vector
      (value-and-tcr-kept
       (lambda ()
         (keep-float-and-call
          1d0
          (lambda () (keep-float-and-vector-around 1d0 #'signal-in-f))))
       (lambda ()
         (ccl::return-from-frame (frame-of-function 'keep-float-around) 42)))
    ((42 2.0d0) t))

  ;; Signals an error when foreign code calls it.  It takes six
  ;; arguments because, on x86-64 other than Windows, the fifth and
  ;; sixth are pushed over the first two words of the frame that held
  ;; the foreign call's arguments.
  (ccl:defcallback signal-in-callback
      (:int a :int b :int c :int d :int e :int f :int)
    (signal-in-f)
    (+ a b c d e f))

  (defun call-out-and-signal ()
    (ccl:ff-call signal-in-callback
                 :int 1 :int 2 :int 3 :int 4 :int 5 :int 6 :int))

  ;; A callback from foreign code lies between.  The frame that held the
  ;; foreign call's arguments no longer says whose it is, so
  ;; RETURN-FROM-FRAME signals an error and changes nothing.
  (deftest ccl.issue#497.callback
      (let* ((before (tcr-nfp-foreign-sp-and-xframe))
             (value (block refused
                      (call-with-error-handler
                       (lambda ()
                         (keep-float-and-call 1d0 #'call-out-and-signal))
                       (lambda ()
                         (handler-case
                             (ccl::return-from-frame
                              (frame-of-function 'keep-float-around) 42)
                           (error () (return-from refused :refused)))))))
             (after (tcr-nfp-foreign-sp-and-xframe)))
        (list value (equal before after)))
    (:refused t)))
