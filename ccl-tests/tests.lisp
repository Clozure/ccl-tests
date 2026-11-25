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
