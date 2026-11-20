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
           (q (* 3 (expt 2 68)))
           (big (expt 2 100))
           (f (compile nil '(lambda (x y)
                             (values nil (catch 'tag (truncate x y)))))))
      (loop for (name got want)
              in (list (list 'truncate (multiple-value-list (truncate 3/4 tiny)) (list q 0))
                       (list 'floor (multiple-value-list (floor 3/4 tiny)) (list q 0))
                       (list 'ceiling (multiple-value-list (ceiling 3/4 tiny)) (list q 0))
                       (list 'round (multiple-value-list (round 3/4 tiny)) (list q 0))
                       (list 'mod (mod 3/4 tiny) 0)
                       (list 'rem (rem 3/4 tiny) 0)
                       (list 'catch (nth-value 1 (funcall f big 1/10)) (* big 10)))
            unless (equal got want)
              collect name))
  nil)
