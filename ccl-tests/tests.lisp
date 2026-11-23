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

;;; The ASIN and ACOS branch cuts to the right of 1 are continuous with
;;; quadrant IV, so for a real X > 1 the imaginary part of (asin x) is
;;; negative and that of (acos x) is positive; to the left of -1 the cuts
;;; are continuous with quadrant II, so (asin x) has a positive and (acos x)
;;; a negative imaginary part.  Above 2^54 or so, ASIN and ACOS took
;;; the sign from the (zero) imaginary part of the argument instead.
(defun asin-acos-large-failures ()
  (let ((failures nil))
    (dolist (x (list 2d0 -2d0 1d10 -1d10 1d16 1.9d16 2d16 1d17 -1d17 1d100
                     1d300 -1d300 1.5d308 2f0 -2f0 1f10 1f18 1f19 -1f19 1f30
                     -1f30 3f38 (expt 10 30) (- (expt 10 30))))
      (let* ((a (asin x))
             (c (acos x))
             (sign (if (plusp x) 1 -1))
             (half-pi (float (/ pi 2) (if (typep x 'single-float) 1f0 1d0))))
        ;; asin: imaginary part has the sign opposite to that of x.
        (unless (and (= (signum (imagpart a)) (- sign))
                     (< (abs (- (abs (realpart a)) half-pi)) 1d-4))
          (push (list :asin x a) failures))
        ;; acos: imaginary part has the sign of x, real part is 0 or pi.
        (unless (and (= (signum (imagpart c)) sign)
                     (< (abs (- (realpart c) (if (plusp x) 0 pi))) 1d-4))
          (push (list :acos x c) failures))
        ;; The magnitude is log(2|x|) either way.
        (when (< 1000 (abs x) 1d300)
          (let ((expected (log (* 2 (abs (float x 1d0))))))
            (when (> (abs (- (abs (imagpart a)) expected)) 1d-3)
              (push (list :magnitude x a) failures))))))
    ;; No -0.0 real part for ACOS of a large positive argument.
    (dolist (x '(1d300 1f30))
      (unless (eql (realpart (acos x)) (float 0 x))
        (push (list :acos-real-part x) failures)))
    (nreverse failures)))

(deftest ccl.asin-acos-large-real
    (asin-acos-large-failures)
  nil)

;;; Complex arguments, and real arguments that don't take the large
;;; argument path, are the same as before.
(deftest ccl.asin-acos-large-complex
    (let ((z1 (complex 1d300 1d0)) (z2 (complex 1d300 -1d0))
          (z3 (complex -1d300 1d0)) (z4 (complex -1d300 -1d0)))
      (list (signum (imagpart (asin z1))) (signum (imagpart (asin z2)))
            (signum (imagpart (asin z3))) (signum (imagpart (asin z4)))
            (signum (imagpart (acos z1))) (signum (imagpart (acos z2)))
            (signum (imagpart (acos z3))) (signum (imagpart (acos z4)))))
  (1d0 -1d0 1d0 -1d0 -1d0 1d0 -1d0 1d0))

(deftest ccl.asin-acos-small-real
    (loop for (x asin-imag acos-imag)
            in '((2d0 -1.3169578969248166d0 1.3169578969248166d0)
                 (-2d0 1.3169578969248166d0 -1.3169578969248166d0)
                 (1d10 -23.7189981105004d0 23.7189981105004d0))
          for a = (asin x)
          for c = (acos x)
          unless (and (< (abs (- (imagpart a) asin-imag)) 1d-9)
                      (< (abs (- (imagpart c) acos-imag)) 1d-9))
            collect x)
  nil)

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
