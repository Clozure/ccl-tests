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
