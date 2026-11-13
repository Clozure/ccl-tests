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

;;; A NaN is unordered with every number, so <, <=, > and >= are all
;;; false when either argument is one, and (not (< x y)) is true.  Each
;;; compare is compiled generic, open-coded for each float type, under
;;; NOT, and with three arguments, and is called with the NaN on each
;;; side.  = and /= are not tested here (Clozure/ccl#440).
(defvar *nan-compare-zero* 0)

(defun nan-compare-failures ()
  (let* ((mode (ccl:get-fpu-mode)))
    (unwind-protect
         (let* ((failures nil))
           (ccl:set-fpu-mode :invalid nil :division-by-zero nil)
           (dolist (type '(double-float single-float t) failures)
             (let* ((one (if (eq type 'double-float) 1d0 1f0))
                    (nan (/ (float *nan-compare-zero* one)
                            (float *nan-compare-zero* one))))
               (dolist (op '(< <= > >=))
                 (dolist (shape '(:plain :not :three))
                   (let* ((fn (compile nil
                                       `(lambda (x y)
                                          (declare (type ,type x y)
                                                   (optimize (speed 3) (safety 0)))
                                          ,(ecase shape
                                             (:plain `(,op x y))
                                             (:not `(not (,op x y)))
                                             (:three `(,op x y y))))))
                          (want (eq shape :not)))
                     (dolist (args (list (list nan one) (list one nan)))
                       (unless (eq want (and (apply fn args) t))
                         (push (list type shape op
                                     (if (eq nan (first args)) :nan-first :nan-second))
                               failures)))))))))
      (apply #'ccl:set-fpu-mode mode))))

(deftest ccl.nan-compare
    (nan-compare-failures)
  nil)
