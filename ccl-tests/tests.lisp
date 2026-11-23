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

;;; (FLOAT ratio 1f0) and (FLOAT ratio 1d0) rounded the numerator and
;;; the denominator to the format before dividing, which is not exact
;;; when either has more bits than the format holds, and a quotient of
;;; 2^24 or more was rounded to an integer first.  The result was off by
;;; one ulp for a quarter of the ratios tried.  It should be the float
;;; nearest to the ratio, which is computed here with integers.
(defvar *float-ratio-state* 12345)

(defun float-ratio-draw (bits)
  (let ((n 0))
    (loop while (< (integer-length n) (+ bits 8))
          do (setq *float-ratio-state*
                   (mod (+ (* *float-ratio-state* 6364136223846793005)
                           1442695040888963407)
                        (expt 2 64)))
             (setq n (logior (ash n 32) (ldb (byte 32 32) *float-ratio-state*))))
    (1+ (ldb (byte bits 0) n))))

;;; The rational value of the float of DIGITS bits nearest to R, ties to
;;; even, for an R in the range of normal floats.
(defun float-ratio-nearest (r digits)
  (let* ((a (abs r))
         (e (- (integer-length (numerator a)) (integer-length (denominator a)))))
    (loop while (>= a (expt 2 e)) do (incf e))
    (loop while (< a (expt 2 (1- e))) do (decf e))
    (let ((scale (expt 2 (- digits e))))
      (* (signum r) (/ (round (* a scale)) scale)))))

(defun float-ratio-failures ()
  (let ((*float-ratio-state* 12345)
        (failures nil))
    (loop for (one digits) in '((1f0 24) (1d0 53))
          do (loop for bits in '(20 25 30 40 60 100)
                   do (loop repeat 300
                            do (let ((r (/ (* (if (oddp (float-ratio-draw 8)) 1 -1)
                                              (float-ratio-draw bits))
                                           (float-ratio-draw bits))))
                                 (when (and (typep r 'ratio)
                                            (/= (rational (float r one))
                                                (float-ratio-nearest r digits)))
                                   (push (list (type-of one) r) failures))))))
    (nreverse failures)))

(deftest ccl.float-ratio-rounding
    (float-ratio-failures)
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
