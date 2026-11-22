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
