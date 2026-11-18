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

;;; A closure that captures more values than arm64's STUR (simm9) reach from
;;; the closure object (32 node slots) made the compiler signal "vinsn
;;; immediate 260 (shift 0) out of range for operand class :SIMM9" while
;;; storing the captured values into the new closure.
(defun wide-closure-vars (n)
  (loop for i below n collect (intern (format nil "V~d" i) :cl-test)))

;;; Create heap closures over N values and read every captured value back,
;;; with no arguments and with enough arguments that some are on the stack.
(deftest ccl.closure.wide
    (loop for n in '(31 32 33 40 100)
          for vars = (wide-closure-vars n)
          for maker = (compile nil `(lambda (base)
                                      (let ,(loop for v in vars
                                                  for i from 0
                                                  collect `(,v (+ base ,i)))
                                        (values (lambda () (list ,@vars))
                                                (lambda (a b c d e)
                                                  (list* a b c d e
                                                         (list ,@vars)))))))
          for expected = (loop for i below n collect (+ 100 i))
          unless (multiple-value-bind (g h) (funcall maker 100)
                   (and (equal (funcall g) expected)
                        (equal (funcall h 1 2 3 4 5)
                               (list* 1 2 3 4 5 expected))))
            collect n)
  nil)

;;; Captured variables that are assigned live in value cells; the cells for
;;; the high slots must be stored, shared, and readable too.
(deftest ccl.closure.wide-setq
    (loop for n in '(33 100)
          for vars = (wide-closure-vars n)
          for last-var = (car (last vars))
          for maker = (compile nil `(lambda (base)
                                      (let ,(loop for v in vars
                                                  for i from 0
                                                  collect `(,v (+ base ,i)))
                                        (values (lambda (x)
                                                  (setq ,last-var x
                                                        ,(first vars) x))
                                                (lambda () (list ,@vars))))))
          unless (multiple-value-bind (setter getter) (funcall maker 0)
                   (funcall setter :new)
                   (equal (funcall getter)
                          (append (list :new)
                                  (loop for i from 1 below (1- n) collect i)
                                  (list :new))))
            collect n)
  nil)
