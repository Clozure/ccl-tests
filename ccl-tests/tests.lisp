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

;;; An infinity is greater (or less) than every integer and ratio, however
;;; large.  BIGNUM-DFLOAT-COMPARE took an infinity for 2^1024, so a bignum
;;; above that compared greater than it, and the compares of a single-float
;;; infinity with a ratio, or with a fixnum that a single-float can't hold,
;;; signaled an error.
(defvar *infinity-compare-one* 1)

(defun infinity-compare-failures ()
  (let* ((mode (ccl:get-fpu-mode)))
    (unwind-protect
         (let* ((failures nil)
                (big (expt 10 400)))
           (ccl:set-fpu-mode :overflow nil)
           (dolist (type '(double-float single-float) failures)
             (let* ((huge (if (eq type 'double-float)
                              most-positive-double-float
                              most-positive-single-float))
                    (inf (* huge (+ 1 *infinity-compare-one*))))
               (dolist (sign '(1 -1))
                 (let ((x (* sign inf)))
                   (dolist (yname '(0 1 -1 most-positive-fixnum most-negative-fixnum
                                    2^64 -2^64 10^400 -10^400 1/3 -1/3
                                    10^400/3 -10^400/3 1/10^400 -1/10^400))
                     ;; The infinity is greater than Y if its sign is +.
                     (let* ((y (ecase yname
                                 ((0 1 -1 1/3 -1/3) yname)
                                 (most-positive-fixnum most-positive-fixnum)
                                 (most-negative-fixnum most-negative-fixnum)
                                 (2^64 (expt 2 64)) (-2^64 (- (expt 2 64)))
                                 (10^400 big) (-10^400 (- big))
                                 (10^400/3 (/ big 3)) (-10^400/3 (- (/ big 3)))
                                 (1/10^400 (/ 1 big)) (-1/10^400 (- (/ 1 big)))))
                            (got (ignore-errors
                                 (list (< x y) (> x y) (<= x y) (>= x y)
                                       (= x y) (/= x y)
                                       (< y x) (> y x) (<= y x) (>= y x)
                                       (= y x) (/= y x)
                                       (eql x (max x y)) (eql x (min x y)))))
                           (want (if (eql sign 1)
                                     '(nil t nil t nil t t nil t nil nil t t nil)
                                     '(t nil t nil nil t nil t nil t nil t nil t))))
                       (unless (equal (mapcar (lambda (v) (and v t)) got) want)
                         (push (list type sign yname) failures)))))))))
      (apply #'ccl:set-fpu-mode mode))))

(deftest ccl.infinity-rational-compare
    (infinity-compare-failures)
  nil)

;;; A NaN is unordered with every integer and ratio, so = is false, /= is
;;; true, and < and > are false, with the NaN on either side.  The compares
;;; of a NaN with a bignum or a ratio, and with a fixnum that a float can't
;;; hold, answered from the RATIONAL or the exponent of the NaN, or (for a
;;; single-float) signaled an error.  The invalid operation trap is masked,
;;; since CCL signals it for a NaN.  (= and /= of a NaN with a float are in
;;; ccl.nan-equality.)
(defvar *nan-rational-compare-zero* 0)

;;; T or NIL, or :ERROR if the call signals an error.
(defun nan-rational-compare-value (fn &rest args)
  (handler-case (and (apply fn args) t)
    (error () :error)))

;;; Y, or a short description of it if it is long.
(defun nan-rational-compare-label (y)
  (if (> (+ (integer-length (numerator y)) (integer-length (denominator y))) 64)
      (list :big (signum y) (integer-length (numerator y)) (integer-length (denominator y)))
      y))

(defun nan-rational-compare-failures ()
  (let* ((mode (ccl:get-fpu-mode)))
    (unwind-protect
         (let* ((failures nil))
           (ccl:set-fpu-mode :invalid nil)
           (dolist (type '(double-float single-float) failures)
             (let* ((one (if (eq type 'double-float) 1d0 1f0))
                    (zero (float *nan-rational-compare-zero* one))
                    (nan (/ zero zero)))
               ;; (- nan) is a NaN with the opposite sign bit.
               (dolist (n (list nan (- nan)))
                 (dolist (y (list 0 1 -1 most-positive-fixnum most-negative-fixnum
                                  (expt 2 64) (- (expt 2 64))
                                  (expt 10 400) (- (expt 10 400))
                                  1/3 -1/3 (/ (expt 10 30) 3) (/ (expt 10 400))))
                   (dolist (op '(= /= < >))
                     (let ((want (eq op '/=)))
                       (unless (and (eq want (nan-rational-compare-value op n y))
                                    (eq want (nan-rational-compare-value op y n)))
                         (push (list type op (if (eq n nan) :nan :-nan) (nan-rational-compare-label y))
                               failures)))))))))
      (apply #'ccl:set-fpu-mode mode))))

(deftest ccl.nan-rational-compare
    (nan-rational-compare-failures)
  nil)

;;; Float and rational compares that don't involve an infinity or a NaN
;;; are the same as before.
(deftest ccl.float-rational-compare
    (loop for (x y order)
            in `((1d0 ,(expt 10 400) <) (1d300 ,(expt 10 400) <)
                 (-1d300 ,(- (expt 10 400)) >) (1d300 ,(expt 10 300) >)
                 (1d0 1/3 >) (-1d0 -1/3 <) (0.5d0 1/2 =) (0.25f0 1/4 =)
                 (1f0 1/3 >) (1f30 ,(expt 10 31) <)
                 (1d0 ,most-positive-fixnum <) (1f0 ,most-positive-fixnum <)
                 (1f10 ,(1- (expt 10 10)) >) (1d20 ,(expt 10 20) =)
                 (0d0 0 =) (-0d0 0 =) (0f0 1/3 <))
          unless (let ((fn (ecase order (< #'<) (> #'>) (= #'=))))
                   (and (funcall fn x y)
                        (funcall (ecase order (< #'>) (> #'<) (= #'=)) y x)
                        (not (funcall (ecase order (< #'>=) (> #'<=) (= #'/=)) x y))))
            collect (list x y order))
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
