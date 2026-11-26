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

;;; The expander of a compiler macro, a type, a long-form DEFSETF or a
;;; DEFINE-SETF-EXPANDER, and the expander of a long-form
;;; DEFINE-METHOD-COMBINATION, are functions of fixed arguments
;;; (WHOLE ENVIRONMENT, and so on), so ARGLIST of them gave the
;;; names of those, and the lambda list the user wrote was lost.  Each
;;; definer now keeps it with the expander, as DEFUN does for a function
;;; (see ccl.defun-arglist.*).  Where to find each expander:
;;;   compiler macro    (compiler-macro-function name)
;;;   type              (ccl::%deftype-expander name)
;;;   defsetf, define-setf-expander
;;;                     (ccl::%setf-method name)
;;;   method combination, long form
;;;                     (cdr (ccl::mci.options (ccl::method-combination-info name)))

(defun definer-arglists-forms (prefix)
  "Definitions of the five kinds, with names starting with PREFIX, as a list of forms."
  (flet ((name (suffix)
           (intern (string-upcase (format nil "~a.~a" prefix suffix)) :cl-test)))
    `((define-compiler-macro ,(name "cm")
          (&whole form a &optional (b 2 b-p) &rest r &key (c 3) ((:dee d) 4)
           &allow-other-keys &environment env &aux (z 1))
        (list a b env b-p r c d z)
        form)
      (deftype ,(name "ty") (a &optional (b 3 b-p) &rest r &key (c 4) &aux (z 5))
        (list b b-p r c z)
        (list 'integer a))
      (defsetf ,(name "acc") (a &optional (b 2) &key (c 3)) (v)
        `(list ,a ,b ,c ,v))
      (define-setf-expander ,(name "expander")
          (a &optional (b 2 b-p) &key (c 3) &environment env)
        (list a b b-p c env)
        (values nil nil (list (gensym)) nil nil))
      (define-method-combination ,(name "mc")
          (&optional (order :most-specific-first order-p) &key (extra 1))
          ((methods ()))
        (list order order-p extra)
        `(call-method ,(first methods))))))

(defun definer-arglists-results (prefix)
  "The lambda lists ARGLIST finds for the definitions made by DEFINER-ARGLISTS-FORMS."
  (flet ((name (suffix)
           (intern (string-upcase (format nil "~a.~a" prefix suffix)) :cl-test)))
    (list (values (ccl:arglist (compiler-macro-function (name "cm"))))
          (values (ccl:arglist (ccl::%deftype-expander (name "ty"))))
          (values (ccl:arglist (ccl::%setf-method (name "acc"))))
          (values (ccl:arglist (ccl::%setf-method (name "expander"))))
          (values (ccl:arglist
                   (cdr (ccl::mci.options
                         (ccl::method-combination-info (name "mc")))))))))

;;; Defined by evaluating the definitions.
(deftest ccl.definer-arglists.eval
    (progn
      (dolist (form (definer-arglists-forms "ccl.definer-arglists.eval"))
        (eval form))
      (definer-arglists-results "ccl.definer-arglists.eval"))
  ((&whole form a &optional (b 2 b-p) &rest r &key (c 3) ((:dee d) 4)
    &allow-other-keys &environment env &aux (z 1))
   (a &optional (b 3 b-p) &rest r &key (c 4) &aux (z 5))
   (a &optional (b 2) &key (c 3))
   (a &optional (b 2 b-p) &key (c 3) &environment env)
   (&optional (order :most-specific-first order-p) &key (extra 1))))

;;; Defined in a file that is compiled and then loaded.
(deftest ccl.definer-arglists.compile-file
    (let* ((file (test-source-file
                  "(in-package :cl-test)~%~{~s~%~}"
                  (definer-arglists-forms "ccl.definer-arglists.file")))
           (fasl (let ((ccl:*fasl-save-definitions* nil))
                   (test-compile file :hide-warnings t))))
      (load fasl)
      (definer-arglists-results "ccl.definer-arglists.file"))
  ((&whole form a &optional (b 2 b-p) &rest r &key (c 3) ((:dee d) 4)
    &allow-other-keys &environment env &aux (z 1))
   (a &optional (b 3 b-p) &rest r &key (c 4) &aux (z 5))
   (a &optional (b 2) &key (c 3))
   (a &optional (b 2 b-p) &key (c 3) &environment env)
   (&optional (order :most-specific-first order-p) &key (extra 1))))

;;; The type, the setf expanders and the method combination still work.
(deftest ccl.definer-arglists.expansions
    (progn
      (dolist (form (definer-arglists-forms "ccl.definer-arglists.use"))
        (eval form))
      (eval '(defgeneric ccl.definer-arglists.use.gf (x)
              (:method-combination ccl.definer-arglists.use.mc)))
      (eval '(defmethod ccl.definer-arglists.use.gf (x) (list x)))
      (values
       (list (typep 8 '(ccl.definer-arglists.use.ty 7))
             (typep 6 '(ccl.definer-arglists.use.ty 7)))
       (funcall (compile nil '(lambda ()
                               (setf (ccl.definer-arglists.use.acc 1 2 :c 3) 4))))
       (funcall (compile nil '(lambda ()
                               (setf (ccl.definer-arglists.use.expander 1) 5))))
       (ccl.definer-arglists.use.gf 6)))
  (t nil)
  (1 2 3 4)
  nil
  (6))

;;; *SAVE-ARGLIST-INFO* turns it off, when the definition is expanded:
;;; ARGLIST then rebuilds the lambda list of the expander, as before.
(deftest ccl.definer-arglists.save-arglist-info-nil
    (let ((ccl:*save-arglist-info* nil))
      (dolist (form (definer-arglists-forms "ccl.definer-arglists.unsaved"))
        (eval form))
      (let ((results (definer-arglists-results "ccl.definer-arglists.unsaved")))
        (every (lambda (lambda-list)
                 (every (lambda (x) (and (symbolp x) (null (symbol-package x))))
                        lambda-list))
               results)))
  t)

;;; The short form of DEFINE-METHOD-COMBINATION has no lambda list; its
;;; options are kept as written, and still are.
(deftest ccl.definer-arglists.short-method-combination
    (progn
      (eval '(define-method-combination ccl.definer-arglists.short-mc
              :operator + :identity-with-one-argument t
              :documentation "A short method combination."))
      (values (ccl::mci.options
               (ccl::method-combination-info 'ccl.definer-arglists.short-mc))
              (documentation 'ccl.definer-arglists.short-mc 'method-combination)))
  (:operator + :identity-with-one-argument t
   :documentation "A short method combination.")
  "A short method combination.")

;;; The docstring of a compiler macro and of a long-form DEFSETF was
;;; dropped (DOCUMENTATION returned NIL for 'COMPILER-MACRO and for
;;; 'SETF); those of DEFINE-SETF-EXPANDER and DEFTYPE were kept.

(defun definer-docstrings-forms (prefix)
  (flet ((name (suffix)
           (intern (string-upcase (format nil "~a.~a" prefix suffix)) :cl-test)))
    `((define-compiler-macro ,(name "cm") (a)
        "Compiler macro doc."
        a)
      (define-compiler-macro (setf ,(name "cm")) (value a)
        "Setf compiler macro doc."
        (list value a))
      (deftype ,(name "ty") (a)
        "Type doc."
        (list 'integer a))
      (defsetf ,(name "acc") (a) (v)
        "Long defsetf doc."
        `(list ,a ,v))
      (define-setf-expander ,(name "expander") (a)
        "Expander doc."
        (list a)
        (values nil nil (list (gensym)) nil nil)))))

(defun definer-docstrings-results (prefix)
  (flet ((name (suffix)
           (intern (string-upcase (format nil "~a.~a" prefix suffix)) :cl-test)))
    (list (documentation (name "cm") 'compiler-macro)
          (documentation `(setf ,(name "cm")) 'compiler-macro)
          (documentation (name "ty") 'type)
          (documentation (name "acc") 'setf)
          (documentation (name "expander") 'setf))))

(deftest ccl.definer-docstrings.eval
    (progn
      (dolist (form (definer-docstrings-forms "ccl.definer-docstrings.eval"))
        (eval form))
      (definer-docstrings-results "ccl.definer-docstrings.eval"))
  ("Compiler macro doc." "Setf compiler macro doc." "Type doc."
   "Long defsetf doc." "Expander doc."))

(deftest ccl.definer-docstrings.compile-file
    (let* ((file (test-source-file
                  "(in-package :cl-test)~%~{~s~%~}"
                  (definer-docstrings-forms "ccl.definer-docstrings.file")))
           (fasl (test-compile file :hide-warnings t)))
      (load fasl)
      (definer-docstrings-results "ccl.definer-docstrings.file"))
  ("Compiler macro doc." "Setf compiler macro doc." "Type doc."
   "Long defsetf doc." "Expander doc."))

;;; The docstring is not a form of the body: DEFSETF still returns the
;;; name and the update form still works, and a body that is only a
;;; string is not taken for a docstring.
(deftest ccl.definer-docstrings.not-a-body-form
    (progn
      (eval '(defsetf ccl.definer-docstrings.body-acc (a) (v)
              "Doc." `(list ,a ,v)))
      (eval '(defsetf ccl.definer-docstrings.string-acc (a) (v) "just a string"))
      (values (eval '(defsetf ccl.definer-docstrings.value-acc (a) (v) "Doc."
                      `(list ,a ,v)))
              (documentation 'ccl.definer-docstrings.body-acc 'setf)
              (documentation 'ccl.definer-docstrings.string-acc 'setf)
              (funcall (compile nil '(lambda ()
                                      (setf (ccl.definer-docstrings.body-acc 1) 2))))))
  ccl.definer-docstrings.value-acc
  "Doc."
  nil
  (1 2))
