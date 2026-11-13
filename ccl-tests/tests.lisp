;;;-*-Mode: LISP; Package: CL-TEST -*-
;;;
;;; CCL-specific tests.  New tests go here; the older ones are still in
;;; ansi-tests/ccl.lsp, which is loaded first, so its helpers (such as
;;; TEST-COMPILE and TEST-SOURCE-FILE) are available here.
;;;
;;; The tests run with the current directory set to ansi-tests/, so a
;;; relative file name in a test names a file there.

(in-package :cl-test)

;;; Like ccl.aref.complex-float-2d-3d in ansi-tests/ccl.lsp, but the object
;;; isn't an array of the right rank at all: the check fails before it ever
;;; looks at the array's flags.
(deftest ccl.aref.simple-array-wrong-object
    (loop for rank in '(2 3)
          for type = `(simple-array double-float
                                    ,(make-list rank :initial-element '*))
          for reader = (compile
                        nil
                        `(lambda (a)
                           (declare (optimize (safety 1)) (type ,type a))
                           (aref a ,@(make-list rank :initial-element 0))))
          nconc (loop for object in (list 3 nil "abc" (make-array 3)
                                          (make-array
                                           (make-list (- 5 rank)
                                                      :initial-element 2)
                                           :element-type 'double-float))
                      collect (handler-case (progn
                                              (funcall reader object)
                                              :no-error)
                                (type-error (c)
                                  (and (eql (type-error-datum c) object)
                                       (equal (type-error-expected-type c)
                                              type))))))
  (t t t t t t t t t t))

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
