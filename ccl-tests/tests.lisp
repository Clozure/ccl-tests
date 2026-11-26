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

;;; The short form of DEFSETF recorded no source location, so
;;; FIND-DEFINITION-SOURCES found nothing for its setf expander, though
;;; the long form and DEFINE-SETF-EXPANDER did.
(defun short-defsetf-source-forms (prefix)
  (flet ((name (suffix)
           (intern (string-upcase (format nil "~a.~a" prefix suffix)) :cl-test)))
    `((defun ,(name "get") (x) (car x))
      (defun ,(name "set") (x v) (setf (car x) v))
      (defun ,(name "get2") (x) (car x))
      (defsetf ,(name "get") ,(name "set"))
      (defsetf ,(name "get2") ,(name "set") "Short doc.")
      (defun ,(name "get3") (x) (car x))
      (defsetf ,(name "get3") (x) (v) `(setf (car ,x) ,v))
      (defun ,(name "get4") (x) (car x))
      (define-setf-expander ,(name "get4") (x)
        (declare (ignore x))
        (values nil nil (list (gensym)) nil nil)))))

(defun short-defsetf-source-results (prefix file)
  "For each setf expander made by SHORT-DEFSETF-SOURCE-FORMS: the number of
sources FIND-DEFINITION-SOURCES gives for it, and whether the first is FILE."
  (flet ((name (suffix)
           (intern (string-upcase (format nil "~a.~a" prefix suffix)) :cl-test)))
    (loop for suffix in '("get" "get2" "get3" "get4")
          for sources = (ccl:find-definition-sources (name suffix)
                                                     'ccl::setf-expander)
          collect (list (length sources)
                        (and sources
                             (equal (truename (ccl:source-note-filename
                                               (cadar sources)))
                                    file))))))

;;; Defined in a file that is compiled and then loaded.
(deftest ccl.short-defsetf-source.compile-file
    (let* ((prefix "ccl.short-defsetf-source.file")
           (file (test-source-file
                  "(in-package :cl-test)~%~{~s~%~}"
                  (short-defsetf-source-forms prefix)))
           (fasl (test-compile file :hide-warnings t)))
      (load fasl)
      (short-defsetf-source-results prefix file))
  ((1 t) (1 t) (1 t) (1 t)))

;;; Defined by loading the source file.
(deftest ccl.short-defsetf-source.load-source
    (let* ((prefix "ccl.short-defsetf-source.src")
           (file (test-source-file
                  "(in-package :cl-test)~%~{~s~%~}"
                  (short-defsetf-source-forms prefix))))
      (load file)
      (short-defsetf-source-results prefix file))
  ((1 t) (1 t) (1 t) (1 t)))

;;; The docstring of a short DEFSETF is kept, and the expanders work.
(deftest ccl.short-defsetf-source.docstring-and-use
    (let* ((prefix "ccl.short-defsetf-source.use")
           (file (test-source-file
                  "(in-package :cl-test)~%~{~s~%~}"
                  (short-defsetf-source-forms prefix))))
      (load (test-compile file :hide-warnings t))
      (flet ((name (suffix)
               (intern (string-upcase (format nil "~a.~a" prefix suffix))
                       :cl-test)))
        (values
         (documentation (name "get") 'setf)
         (documentation (name "get2") 'setf)
         (let ((cell (list 1)))
           (eval `(setf (,(name "get") ',cell) 2))
           (eval `(setf (,(name "get2") ',cell) (1+ (,(name "get2") ',cell))))
           cell))))
  nil "Short doc." (3))

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
