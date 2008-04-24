;;;-*-Mode: LISP; Package: CL-TEST -*-
;;;
;;;   Copyright (C) 2008 Clozure Associates

(in-package :cl-test)


;;; Regression tests

(deftest ccl.40199  ;; fixed in r9116 and r9121
    (when (equalp (let ((*print-pretty* t))
                    (format nil "~a" (make-array nil :initial-element 0)))
                  "#0A0")
      :good)
  :good)

(deftest ccl.40492 ;; fixed in r9134 and r9131
    (let (obj (slot (gensym)))
      (eval `(defclass ccl.40492 ()
               ((,slot :accessor ,slot :initform :good))))
      (setq obj (make-instance 'ccl.40492))
      (ccl::%snap-reader-method (symbol-function slot))
      (unwind-protect
           (let ((*trace-output* (make-broadcast-stream))) ;; don't care about trace output
             (ccl:trace-function slot)
             (funcall slot obj))
        (eval `(untrace ,slot))))
  :good)

(deftest ccl.40207  ;; fixed in r9163 and r9165
    ;; Check that these compile-time errors don't abort compilation.
    (and (typep (lambda (x) (setq x)) 'function)
         (typep (lambda (x) (setf x)) 'function)
         (typep (lambda (((foo))) foo) 'function)
         :good)
  :good)

(deftest ccl.40927  ;; fixed in r9183 and r9184
    (let ((s (make-string-output-stream))
          (line1 "Line1
")
          (line2 "Line2"))
      (count #\Newline (format nil "~a~&~a" line1 line2)))
  1)

(defstruct ccl.40055 (a 0 :type integer))

(deftest ccl.40055 ;; fixed in r9237 and r9240
    (locally
        (declare (optimize (safety 3)))
      (and (signals-error (make-ccl.40055 :a nil) type-error)
           (signals-error (setf (ccl.40055-a (make-ccl.40055)) nil) type-error)))
  t)

(defclass ccl.bug#285 () ())

(defmethod initialize-instance ((c ccl.bug#285) &rest args)
  (declare (optimize (safety 3)))
  (apply #'call-next-method c args))

(deftest ccl.bug#285
    (typep (make-instance 'ccl.bug#285) 'ccl.bug#285)
  t)

(deftest ccl.bug#286
    (and (typep (lambda () (typep nil '(or ccl.bug#286-unknown-type-1 null))) 'function)
         (typep (lambda () (ccl:require-type nil '(or ccl.bug#286-unknown-type-2 null))) 'function)
         :good)
  :good)
