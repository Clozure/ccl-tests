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

;;; SCALE-FLOAT of a denormal by a positive scale returned garbage (0.0,
;;; an infinity, or a wrong number) when the scale was too big for the
;;; scale factor to be a float, and by a very negative scale too; it lost
;;; the sign of a zero that came from underflow; and it signaled a
;;; type error for a scale that is not a fixnum.  Each result is compared
;;; with the one computed from the rational value of the float.
(defun scale-float-expected (x n)
  ;; The expected result as a rational, :ZERO, :OVERFLOW (an infinity if
  ;; the trap is masked, else an error), or X itself when it is a zero.
  (let* ((double (typep x 'double-float))
         (digits (if double 53 24))
         (emin (if double -1074 -149))
         (limit (expt 2 (if double 1024 128)))
         (r (* (abs (rational x)) (expt 2 (max -5000 (min 5000 n))))))
    (if (zerop r)
        :zero
        (let* ((e (- (integer-length (numerator r)) (integer-length (denominator r)))))
          (loop while (>= r (expt 2 e)) do (incf e))
          (loop while (< r (expt 2 (1- e))) do (decf e))
          (let* ((lsb (max (- e digits) emin))
                 (v (* (round (* r (expt 2 (- lsb)))) (expt 2 lsb))))
            (cond ((>= v limit) :overflow)
                  ((zerop v) :zero)
                  (t v)))))))

(defun scale-float-failures ()
  (let* ((mode (ccl:get-fpu-mode))
         (failures nil)
         (xs (list least-positive-single-float
                   (* 12345 least-positive-single-float)
                   (* 8388607 least-positive-single-float)
                   least-positive-normalized-single-float
                   1f0 1.5f0 most-positive-single-float
                   least-positive-double-float
                   (* 123456789 least-positive-double-float)
                   least-positive-normalized-double-float
                   1d0 1.5d0 most-positive-double-float))
         (ns (list (- (expt 2 100)) -5000 -3000 -1200 -1075 -1074 -300 -200 -150
                   -149 -127 -126 -125 -24 -1 0 1 24 125 126 127 128 129 140
                   149 150 300 1023 1024 1074 1075 1100 3000 5000 (expt 2 60)
                   (expt 2 100))))
    (unwind-protect
         (dolist (masked '(nil t) (nreverse failures))
           (ccl:set-fpu-mode :overflow (not masked))
           (dolist (x xs)
             (dolist (sign '(1 -1))
               (dolist (n ns)
                 (let* ((x (* sign x))
                        (want (scale-float-expected x n))
                        (got (handler-case (scale-float x n)
                               (floating-point-overflow () :overflow)
                               (error () :error)))
                        (ok (case want
                              (:zero (and (floatp got) (zerop got)
                                          (eql (minusp (float-sign got)) (minusp sign))))
                              (:overflow (if masked
                                             (and (floatp got)
                                                  (> (abs got) (if (typep x 'double-float)
                                                                   most-positive-double-float
                                                                   most-positive-single-float))
                                                  (eql (minusp got) (minusp sign)))
                                             (eq got :overflow)))
                              (t (and (floatp got)
                                      (eql (ignore-errors (rational got))
                                           (* sign want)))))))
                   (unless ok
                     (push (list masked x n got) failures)))))))
      (apply #'ccl:set-fpu-mode mode))))

(deftest ccl.scale-float
    (scale-float-failures)
  nil)

;;; SCALE-FLOAT treated an infinity or a NaN as a normal number with a
;;; big exponent.  Scaling an infinity down gave a finite number or zero,
;;; scaling one by 0 signaled an overflow, and a NaN scaled down came
;;; back finite.  The argument should come back unchanged, whether or not
;;; the overflow trap is enabled.  A NaN never compares equal, so the
;;; test compares bits.
(defun scale-float-inf-nan-failures ()
  (flet ((bits (x)
           (etypecase x
             (double-float (multiple-value-list (ccl::double-float-bits x)))
             (single-float (ccl::single-float-bits x)))))
    (let ((mode (ccl:get-fpu-mode))
          (failures nil)
          (xs (list (ccl::double-float-from-bits #x7ff00000 0)
                    (ccl::double-float-from-bits #xfff00000 0)
                    (ccl::double-float-from-bits #x7ff80000 0)
                    (ccl::host-single-float-from-unsigned-byte-32 #x7f800000)
                    (ccl::host-single-float-from-unsigned-byte-32 #xff800000)
                    (ccl::host-single-float-from-unsigned-byte-32 #x7fc00000)))
          (ns (list 0 100 -100 5000 -5000 (expt 2 100) (- (expt 2 100)))))
      (unwind-protect
           (dolist (masked '(nil t) (nreverse failures))
             (ccl:set-fpu-mode :overflow (not masked))
             (dolist (x xs)
               (dolist (n ns)
                 (let ((got (handler-case (scale-float x n)
                              (floating-point-overflow () :overflow)
                              (error () :error))))
                   (unless (and (floatp got) (equal (bits got) (bits x)))
                     (push (list masked (bits x) n got) failures))))))
        (apply #'ccl:set-fpu-mode mode)))))

(deftest ccl.scale-float.inf-nan
    (scale-float-inf-nan-failures)
  nil)

;;; (FLOAT ratio 1f0) and (FLOAT ratio 1d0) should return the float
;;; nearest to the ratio, ties to even.  They rounded the numerator and
;;; the denominator to the format before dividing, and rounded a large
;;; quotient to an integer before converting it.  Each test below takes
;;; one family of ratios aimed at one such mistake, and compares each
;;; ratio and its negation with the nearest float computed with
;;; integers.

;;; The rational value of the float of DIGITS bits nearest to R, ties to
;;; even, for an R in the range of normal floats.
(defun float-ratio-nearest (r digits)
  (let* ((a (abs r))
         (e (- (integer-length (numerator a)) (integer-length (denominator a)))))
    (loop while (>= a (expt 2 e)) do (incf e))
    (loop while (< a (expt 2 (1- e))) do (decf e))
    (let ((scale (expt 2 (- digits e))))
      (* (signum r) (/ (round (* a scale)) scale)))))

;;; The ratios among RATIOS, and their negations, that (FLOAT r ONE)
;;; does not convert to the nearest float of DIGITS bits.
(defun float-ratio-failures (one digits ratios)
  (loop for r in ratios
        nconc (loop for x in (list r (- r))
                    unless (= (rational (float x one))
                              (float-ratio-nearest x digits))
                      collect x)))

;;; The case that was reported.
(deftest ccl.float-ratio-rounding.reported
    (float 6069/26411177 1f0)
  2.2978908e-4)

;;; A numerator and a denominator wider than the significand, which are
;;; not exact in the format.  Rounding each of them and then the
;;; quotient got about a quarter of these wrong.
(deftest ccl.float-ratio-rounding.wide-parts
    (append (float-ratio-failures 1f0 24
                                  (loop for i from 1 to 200
                                        collect (/ (+ (expt 2 24) i)
                                                   (expt 3 17))))
            (float-ratio-failures 1d0 53
                                  (loop for i from 1 to 200
                                        collect (/ (+ (expt 2 60) i)
                                                   (expt 3 37)))))
  nil)

;;; Quotients near 2^70, each 1/3 above the halfway point between a
;;; float with an even significand and the next float up, so that the
;;; upper one is nearest.  Rounding to an integer first lands exactly
;;; on the halfway point, and the tie then goes to the even one, which
;;; is wrong for all of these.
(defun float-ratio-above-ties (digits)
  (let ((ulp (expt 2 (- 71 digits))))
    (loop for m below 100
          collect (+ (expt 2 70) (* 2 m ulp) (/ ulp 2) 1/3))))

(deftest ccl.float-ratio-rounding.above-ties
    (append (float-ratio-failures 1f0 24 (float-ratio-above-ties 24))
            (float-ratio-failures 1d0 53 (float-ratio-above-ties 53)))
  nil)

;;; Ratios within a few units of a power of two over a power of two.
;;; Rounding such a quotient can carry it into the next binade, as with
;;; (2^61-1)/(2^60+1), whose nearest double is 2.0d0; a conversion that
;;; doesn't allow for the carry gets 1.0d0.  The negations include
;;; numerators that are negative powers of two, whose INTEGER-LENGTH is
;;; one less than that of their absolute value.
(defun float-ratio-near-powers-of-two ()
  (loop for (a b) in '((31 30) (40 40) (61 60) (80 80) (120 50) (50 120))
        nconc (loop for k from -2 to 2
                    nconc (loop for j from -2 to 2
                                collect (/ (+ (expt 2 a) k)
                                           (+ (expt 2 b) j))))))

(deftest ccl.float-ratio-rounding.boundary
    (append (float-ratio-failures 1f0 24 (float-ratio-near-powers-of-two))
            (float-ratio-failures 1d0 53 (float-ratio-near-powers-of-two)))
  nil)

;;; (FLOAT-SIGN X Y) took the sign of Y from MINUSP, which is false for
;;; -0.0, so it got the sign wrong when Y was a negative zero:
;;; (FLOAT-SIGN -1.0 -0.0) returned 0.0, and (FLOAT-SIGN 1.0 -0.0)
;;; returned -0.0.  The result should have the sign of X, and the
;;; format and magnitude of Y.  -0.0 and 0.0 are EQUALP, so the test
;;; compares signs with one-argument FLOAT-SIGN.
(defparameter *float-sign-args*
  '(1.0 -1.0 0.0 -0.0 2.5 -2.5 1d0 -1d0 0d0 -0d0 2.5d0 -2.5d0))

(deftest ccl.float-sign.negative-zero
    (loop for x in *float-sign-args*
          nconc (loop for y in *float-sign-args*
                      for r = (float-sign x y)
                      unless (and (= (float-sign r) (float-sign x))
                                  (= (abs r) (abs y))
                                  (eql (float-digits r) (float-digits y)))
                        collect (list x y r)))
  nil)

;;; Converting an integer just below 2^1024 to a double-float, or just
;;; below 2^128 to a single-float, rounds up to 2^1024 or 2^128, which
;;; is too big.  FLOAT returned an infinity without signaling an overflow
;;; (it did signal one for the integer 2^1024 itself).  The integers that
;;; round up are the ones at or above 2^1024 - 2^970 and 2^128 - 2^103;
;;; those just below must still convert.
(defun float-bignum-overflow-failures ()
  (let ((mode (ccl:get-fpu-mode))
        (failures nil))
    (unwind-protect
         (progn
           (ccl:set-fpu-mode :overflow t)
           (loop for (one digits emax) in '((1d0 53 1024) (1f0 24 128))
                 do (let ((limit (- (expt 2 emax) (expt 2 (- emax digits 1))))
                          (largest (- (expt 2 emax) (expt 2 (- emax digits)))))
                      (dolist (delta (list 0 1 2 100 (expt 2 (- emax digits 2))))
                        (progn
                          (dolist (sign '(1 -1))
                            (dolist (n (list (- limit delta) (+ limit delta)))
                              (let* ((n (* sign n))
                                     (overflow (>= (abs n) limit))
                                     (got (handler-case (float n one)
                                            (floating-point-overflow () :overflow))))
                                (unless (if overflow
                                            (eq got :overflow)
                                            (eql got (* sign (float largest one))))
                                  (push (list (type-of one) n got) failures))))))))))
      (apply #'ccl:set-fpu-mode mode))
    (nreverse failures)))

(deftest ccl.float-bignum-overflow
    (float-bignum-overflow-failures)
  nil)

;;; A ratio whose quotient rounds up to 2^1024 or 2^128 must signal an
;;; overflow too.  Ratios are converted by different code.
(deftest ccl.float-bignum-overflow.ratio
    (flet ((try (x one)
             (handler-case (float x one)
               (floating-point-overflow () :overflow))))
      (list (try (- (expt 2 1024) (/ (expt 2 970) 3)) 1d0)
            (try (- (expt 2 128) (/ (expt 2 103) 3)) 1f0)))
  (:overflow :overflow))

;;; With the overflow trap masked, FLOAT of a bignum or a ratio that is
;;; too big signaled FLOATING-POINT-OVERFLOW anyway.  It should return an
;;; infinity of the same sign, as SCALE-FLOAT does, and still signal with
;;; the trap enabled.
(defun float-overflow-trap-failures ()
  (let ((mode (ccl:get-fpu-mode))
        (failures nil))
    (unwind-protect
         (dolist (masked '(nil t) (nreverse failures))
           (ccl:set-fpu-mode :overflow (not masked))
           (loop for (one digits emax inf)
                   in (list (list 1d0 53 1024
                                  ccl::double-float-positive-infinity)
                            (list 1f0 24 128
                                  ccl::single-float-positive-infinity))
                 do (dolist (x (list (- (expt 2 emax)
                                        (expt 2 (- emax digits 1)))
                                     (expt 2 emax)
                                     (expt 2 5000)
                                     (- (expt 2 emax)
                                        (/ (expt 2 (- emax digits 1)) 3))
                                     (/ (expt 2 (+ emax 100)) 3)))
                      (dolist (sign '(1 -1))
                        (let* ((x (* sign x))
                               (got (handler-case (float x one)
                                      (floating-point-overflow () :overflow)
                                      (error () :error))))
                          (unless (if masked
                                      (eql got (* sign inf))
                                      (eq got :overflow))
                            (push (list masked (type-of one) x got)
                                  failures)))))))
      (apply #'ccl:set-fpu-mode mode))))

(deftest ccl.float-overflow-trap
    (float-overflow-trap-failures)
  nil)

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

;;; A callback whose arguments do not all fit in registers reads the rest
;;; from its caller's stack.  Darwin/arm64 packs stack arguments at their
;;; natural size (4 bytes for a single-float, 1 for a char), but callbacks
;;; read each one from an 8-byte slot, so they got the wrong values.
(defvar *issue#735-args* nil)

(ccl:defcallback issue#735-singles
    (:single-float f0 :single-float f1 :single-float f2 :single-float f3
     :single-float f4 :single-float f5 :single-float f6 :single-float f7
     :single-float f8 :single-float f9 :single-float f10 :single-float f11
     :void)
  (setq *issue#735-args* (list f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11)))

(deftest ccl.issue#735.singles
    (progn
      (setq *issue#735-args* nil)
      (ccl:ff-call issue#735-singles
                   :single-float 1f0 :single-float 2f0 :single-float 3f0
                   :single-float 4f0 :single-float 5f0 :single-float 6f0
                   :single-float 7f0 :single-float 8f0 :single-float 9f0
                   :single-float 10f0 :single-float 11f0 :single-float 12f0
                   :void)
      *issue#735-args*)
  (1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0 9f0 10f0 11f0 12f0))

;;; Eight integers fill the integer argument registers, so everything after
;;; them goes on the stack, in arguments of several sizes.
(ccl:defcallback issue#735-mixed
    (:signed-fullword a0 :signed-fullword a1 :signed-fullword a2
     :signed-fullword a3 :signed-fullword a4 :signed-fullword a5
     :signed-fullword a6 :signed-fullword a7
     :signed-byte c :signed-halfword s :signed-fullword i
     :signed-doubleword ll :signed-byte c2 :unsigned-halfword us
     :void)
  (setq *issue#735-args* (list a0 a1 a2 a3 a4 a5 a6 a7 c s i ll c2 us)))

(deftest ccl.issue#735.mixed
    (progn
      (setq *issue#735-args* nil)
      (ccl:ff-call issue#735-mixed
                   :signed-fullword 1 :signed-fullword 2 :signed-fullword 3
                   :signed-fullword 4 :signed-fullword 5 :signed-fullword 6
                   :signed-fullword 7 :signed-fullword 8
                   :signed-byte -9 :signed-halfword -1000
                   :signed-fullword 100000 :signed-doubleword -10000000000
                   :signed-byte 11 :unsigned-halfword 65000
                   :void)
      *issue#735-args*)
  (1 2 3 4 5 6 7 8 -9 -1000 100000 -10000000000 11 65000))

;;; Parsing a type that combined a MEMBER type with (SATISFIES P) called
;;; P on the members and kept the answers in the parsed type, which is
;;; cached.  After P was redefined, TYPEP, UPGRADED-ARRAY-ELEMENT-TYPE
;;; and compiled code still followed the old P.  Each test parses its
;;; type while the old P is defined, then redefines P.  The types are in
;;; variables so that TYPEP looks them up at run time.

(defparameter *issue#754-type* '(and (or null bit) (satisfies issue#754-p1)))

(deftest ccl.issue#754
    (progn
      (setf (fdefinition 'issue#754-p1) #'integerp)
      (typep nil *issue#754-type*)
      (setf (fdefinition 'issue#754-p1)
            (lambda (x) (or (null x) (integerp x))))
      (values (typep nil *issue#754-type*)
              (upgraded-array-element-type *issue#754-type*)))
  t
  t)

;;; The same, for a union: (OR NULL (SATISFIES P)) was parsed as
;;; (SATISFIES P) when P accepted NIL.
(defparameter *issue#754-union-type* '(or null (satisfies issue#754-p2)))

(deftest ccl.issue#754.union
    (progn
      (setf (fdefinition 'issue#754-p2)
            (lambda (x) (or (null x) (integerp x))))
      (typep nil *issue#754-union-type*)
      (setf (fdefinition 'issue#754-p2) #'integerp)
      (typep nil *issue#754-union-type*))
  t)

;;; Code compiled while the old P was defined made a bit vector, which
;;; can't hold NIL.
(deftest ccl.issue#754.compiled
    (progn
      (setf (fdefinition 'issue#754-p3) #'integerp)
      (let ((make (test-compile
                   '(lambda ()
                     (make-array 4 :element-type
                                 '(and (or null bit) (satisfies issue#754-p3))
                                 :initial-element 1)))))
        (setf (fdefinition 'issue#754-p3)
              (lambda (x) (or (null x) (integerp x))))
        (let ((a (funcall make)))
          (setf (aref a 0) nil)
          (aref a 0))))
  nil)

;;; The fix must not keep run-time type checks from calling P.
(defun issue#754-p4 (x) (eq x :ok))

(deftest ccl.issue#754.require-type
    (values (funcall 'ccl:require-type :ok '(satisfies issue#754-p4))
            (handler-case (funcall 'ccl:require-type :bad
                                   '(satisfies issue#754-p4))
              (type-error () :type-error)))
  :ok
  :type-error)

(defclass issue#754-class ()
  ((slot :initarg :slot :type (satisfies issue#754-p4))))

(deftest ccl.issue#754.slot-type
    (values (slot-value (make-instance 'issue#754-class :slot :ok) 'slot)
            (handler-case (progn (make-instance 'issue#754-class :slot :bad)
                                 :no-error)
              (error () :error)))
  :ok
  :error)

;;; CL functions can't be redefined, so parsing still calls them.
;;; (Numeric members become ranges before this, so use symbols.)
(deftest ccl.issue#754.cl-predicate
    (subtypep '(and (member :a :b foo) (satisfies keywordp)) '(member :a :b))
  t
  t)

;;; Without calling P, SUBTYPEP can't tell whether this is a subtype, so
;;; it mustn't say for sure that it isn't.  It did, because it compared
;;; the intersections' parts instead of what they denote.
(defun issue#754-p5 (x) (keywordp x))

(deftest ccl.issue#754.subtypep
    (multiple-value-bind (subtypep certain)
        (subtypep '(and (member :a :b foo) (satisfies issue#754-p5))
                  '(member :a :b))
      (or subtypep (not certain)))
  t)

;;; On darwinarm64 a single-float HFA that does not fit in the
;;; remaining v registers goes on the stack at 4-byte alignment and its
;;; own size, not in 8-byte slots as in AAPCS64.  The callback here
;;; takes the same bytes as separate single-floats, which Darwin also
;;; packs at 4 bytes each: p, the HFA's members and z land at stack
;;; offsets 0, 4, 8, ... in both cases.
#+darwinarm64-target
(progn
(ccl:def-foreign-type nil
    (:struct :ccl-test-float3
      (:a :float)
      (:b :float)
      (:c :float)))

(defvar *ccl-test-stacked-float-args* nil)

(ccl:defcallback ccl-test-stacked-floats-callback
    (:single-float f0 :single-float f1 :single-float f2 :single-float f3
     :single-float f4 :single-float f5 :single-float f6 :single-float f7
     :single-float p :single-float a :single-float b :single-float c
     :single-float z :void)
  (setq *ccl-test-stacked-float-args*
        (list f0 f1 f2 f3 f4 f5 f6 f7 p a b c z)))

;;; Eight floats fill s0-s7, then p, the HFA and z go on the stack.
(deftest ccl.ff-call.stacked-float-hfa
    (ccl:rlet ((h :ccl-test-float3 :a 31f0 :b 32f0 :c 33f0))
      (setq *ccl-test-stacked-float-args* nil)
      (ccl:ff-call ccl-test-stacked-floats-callback
                   :single-float 1f0 :single-float 2f0 :single-float 3f0
                   :single-float 4f0 :single-float 5f0 :single-float 6f0
                   :single-float 7f0 :single-float 8f0
                   :single-float 9f0 (:struct :ccl-test-float3) h
                   :single-float 10f0 :void)
      *ccl-test-stacked-float-args*)
  (1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0 9f0 31f0 32f0 33f0 10f0))

;;; Six floats leave two v registers, too few for the three-member HFA,
;;; so it goes on the stack, s6 and s7 are left unused (AAPCS64 C.3),
;;; and z follows the HFA on the stack.  The callback names s6 and s7
;;; as g6 and g7 and ignores them.
(defvar *ccl-test-stacked-float-args-2* nil)

(ccl:defcallback ccl-test-stacked-floats-callback-2
    (:single-float f0 :single-float f1 :single-float f2 :single-float f3
     :single-float f4 :single-float f5 :single-float g6 :single-float g7
     :single-float a :single-float b :single-float c :single-float z :void)
  (declare (ignore g6 g7))
  (setq *ccl-test-stacked-float-args-2* (list f0 f1 f2 f3 f4 f5 a b c z)))

(deftest ccl.ff-call.stacked-float-hfa-wastes-v-registers
    (ccl:rlet ((h :ccl-test-float3 :a 31f0 :b 32f0 :c 33f0))
      (setq *ccl-test-stacked-float-args-2* nil)
      (ccl:ff-call ccl-test-stacked-floats-callback-2
                   :single-float 1f0 :single-float 2f0 :single-float 3f0
                   :single-float 4f0 :single-float 5f0 :single-float 6f0
                   (:struct :ccl-test-float3) h
                   :single-float 10f0 :void)
      *ccl-test-stacked-float-args-2*)
  (1f0 2f0 3f0 4f0 5f0 6f0 31f0 32f0 33f0 10f0))
)

(deftest ccl.issue#546
    (let* ((min-subnormal-exp
             (nth-value 1 (integer-decode-float least-positive-double-float))) ;; -1074
           (min-subnormal-exp-short
             (- ccl::ieee-single-float-normal-exponent-min
                ccl::ieee-single-float-bias
                ccl::ieee-single-float-mantissa-width)) ;; -149
           (cases (list
                   ;; double-float
                   '(expt 2d0 -1024) ;; issue
                   `(expt 2d0 ,min-subnormal-exp)
                   `(expt 2d0 (1- ,min-subnormal-exp))
                   '(expt 1.8964981665016996d0 -1)
                   '(expt 1d0 (- (expt 10 400)))
                   '(expt -1d0 (- (expt 10 400)))
                   '(expt -1d0 (- 1 (expt 10 400)))
                   '(expt 1.8964981665016996d0 -21)
                   '(expt 2d0 (ash 1 10))
                   '(expt 0.20d0 (ash 1 10))

                   ;; short-float
                   `(expt 2.0s0 ,min-subnormal-exp-short)
                   `(expt 2.0s0 (1- ,min-subnormal-exp-short))
                   '(expt 1.55465411564779874s0 -1)
                   '(expt 1s0 (- (expt 10 400)))
                   '(expt -1s0 (- (expt 10 400)))
                   '(expt -1s0 (- 1 (expt 10 400)))
                   '(expt 1.55465411564779874s0 -21)
                   '(expt 2s0 (ash 1 7))
                   '(expt 0.20s0 (ash 1 7))))
           (expects (list
                     ;; double-float
                     5.562684646268003d-309
                     least-positive-double-float
                     0.0D0
                     (/ 1.8964981665016996d0)
                     1d0
                     1d0
                     -1d0
                     (/ (expt 1.8964981665016996d0 21))
                     :floating-point-overflow
                     0.0D0

                     ;; short-float
                     least-positive-short-float
                     0.0s0
                     (/ 1.55465411564779874s0)
                     1.0s0
                     1.0s0
                     -1.0s0
                     (/ (expt 1.55465411564779874s0 21))
                     :floating-point-overflow
                     0.0s0)))
      (loop for x in cases
            for got in expects
            unless (eql got (handler-case (eval x)
                              (floating-point-overflow (e)
                                :floating-point-overflow)))
              collect x))
  nil)
