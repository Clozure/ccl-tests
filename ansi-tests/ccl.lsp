;;;-*-Mode: LISP; Package: CL-TEST -*-
;;;
;;;   Copyright (C) 2008 Clozure Associates

(in-package :cl-test)

(defun test-source-file (format-string &rest format-args)
  (let ((file "temp.dat"))
    (with-open-file (s file :direction :output :if-exists :supersede)
      (apply #'format s format-string format-args)
      (terpri s)
      (truename s))))

(defun test-compile (lambda-or-file &key suppress-warnings (safety 1) load)
  ;; Compile in a more-or-less standard environment
  (let ((ccl::*suppress-compiler-warnings* suppress-warnings)
        (ccl::*nx-speed* 1)
        (ccl::*nx-space* 1)
        (ccl::*nx-safety* safety)
        (ccl::*nx-cspeed* 1)
        (ccl::*nx-debug* 1))
    (if (consp lambda-or-file)
      (compile nil lambda-or-file)
      (compile-file lambda-or-file :load load))))

;;; CCL-specific regression tests, for CCL-specific behavior.

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

(deftest ccl.bug#235
    (handler-case
        (test-compile '(lambda (x)
                        (make-array x :element-type 'ccl.bug#235-unknown-type)))
      (warning (c) (when (typep c 'ccl::compiler-warning)
                     (ccl::compiler-warning-warning-type c))))
  :unknown-type-declaration)


(defclass ccl.bug#285 () ())

(defmethod initialize-instance ((c ccl.bug#285) &rest args)
  (declare (optimize (safety 3)))
  (apply #'call-next-method c args))

(deftest ccl.bug#285
    (typep (make-instance 'ccl.bug#285) 'ccl.bug#285)
  t)

(deftest ccl.bug#286
    (and (test-compile '(lambda ()
                         (typep nil '(or ccl.bug#286-unknown-type-1 null)))
                       :suppress-warnings t)
         (test-compile '(lambda ()
                         (ccl:require-type nil '(or ccl.bug#286-unknown-type-2 null)))
                       :suppress-warnings t)
         :no-crash)
  :no-crash)


(deftest ccl.bug#287
    (progn
      (defmethod ccl.bug#287 (x) x)
      (trace ccl.bug#287)
      (let ((*trace-output* (make-broadcast-stream))) ;; don't care about trace output
        (prog1
            (ccl.bug#287 :no-crash)
          (untrace))))
  :no-crash)


(deftest ccl.41226
    (let ((file (test-source-file "(defmacro ccl.41226 (x) (eq (caar x)))")))
      (test-compile file :suppress-warnings t)
      :no-crash)
  :no-crash)

(deftest ccl.bug#288
    (let ((file (test-source-file "(prog1 (declare (ignore foo)))")))
      (test-compile file :suppress-warnings t)
      :no-crash)
  :no-crash)

(deftest ccl.bug#288-1 ;; follow-on bug, not really the same
    (let ((file (test-source-file "(defun cl-test::ccl.bug#288-1-fn ((x integer)) x)")))
      (test-compile file :suppress-warnings t :load t)
      (handler-case
	  (progn (ccl.bug#288-1-fn 17) :no-warnings)
	(program-error (c) (if (search "(X INTEGER)" (princ-to-string c)) :lambda-list-error c))))
  :lambda-list-error)

(deftest ccl.40055-1
    (let ((file (test-source-file "

 (defclass ccl.40055-1-class () ())
 (eval-when (eval compile load)
  (defstruct ccl.40055-1-struct (slot nil :type (or ccl.40055-1-class null))))
 (defun ccl.40055-1-fn ()
   (make-array 0 :element-type 'ccl.40055-1-struct))
 ")))
      (handler-case
          (progn (test-compile file) :no-warnings)
        (warning (c) (format nil "~a" c))))
  :no-warnings)

(deftest ccl.40055-2
    (let ((file (test-source-file "

 (defclass ccl.40055-2-class () ())
 (defstruct ccl.40055-2-struct (slot nil :type (or ccl.40055-2-class null)))
 (defun ccl.40055-2-class-arr ()
   (make-array 0 :element-type 'ccl.40055-2-class))
 (defun ccl.40055-2-struct-arr ()
   (make-array 0 :element-type 'ccl.40055-2-struct))
 (defun ccl.40055-2-struct-arr-2 ()
   (make-array 0 :element-type '(or (member 17 32) ccl.40055-2-struct)))
 (defun ccl.40055-2-fn (x) (setf (ccl.40055-2-struct-slot x) nil))
 ")))
      (handler-case
          (progn (test-compile file) :no-warnings)
        (warning (c) c)))
  :no-warnings)


(deftest ccl.40055-3
    (let ((file (test-source-file "
 (defclass ccl.40055-3-class () ())
 (defun ccl.40055-3-cfn () (require-type nil '(or ccl.40055-3-class null)))
 (defstruct ccl.40055-3-struct () ())
 (defun ccl.40055-3-rfn () (require-type nil '(or ccl.40055-3-struct null)))")))
      (handler-case
          (progn (test-compile file) :no-warnings)
        (warning (c) c)))
  :no-warnings)

(deftest ccl.bug#289
    (let ((file (test-source-file "
 (defclass ccl.bug#289-meta (standard-class) ())
 (defclass ccl.bug#289-class () () (:metaclass ccl.bug#289-meta))")))
      (test-compile file)
      :no-crash)
  :no-crash)

(deftest ccl.bug#295
    (let ((file (test-source-file "
  (defun outer-fun ()
     (defun inner-fun () nil)
     (inner-fun))")))
      (handler-case (progn (test-compile file :safety 3) :no-warnings)
        (warning (c) c)))
  :no-warnings)


(deftest ccl.41836  ;; fixed in r9391
    (let ((file (test-source-file "
  (defvar *a* 1)
  (defvar *b* (load-time-value *a*))")))
      (handler-case (progn (test-compile file) :no-warnings)
        (warning (c) c)))
  :no-warnings)


(deftest ccl.42698  ;; fixed in r9589/r9590
    (handler-case (schar "abc" -1) ;; used to crash hard
      (error () :error))
  :error)

(deftest ccl.42232-1
    (let ((file (test-source-file "
  (defun ccl.42232-1 (foo)
    (declare (ignore foo))
    foo)")))
      (handler-case (progn (test-compile file) :no-warnings)
        (warning (c) :warning)))
  :warning)

(deftest ccl.42232-2
    (let ((file (test-source-file "
  (defun ccl.42232-2 ()
    (declare (ignore bar)))")))
      (handler-case (progn (test-compile file) :no-warnings)
        (warning (c) :warning)))
  :warning)

(deftest ccl.42830
    (let ((*standard-output* (make-broadcast-stream)))
      (defun cl-user::ccl.42830 (stream int colon-p at-sign-p)
        (declare (ignore at-sign-p colon-p))
        (check-type int integer)
        (write int :stream stream))
      (defun test-ccl.42830 (a b stream)
        (format stream "~A ~/ccl.42830/" a b))
      (and (eq (test-ccl.42830 "a" 1 t) nil)
           (string-equal (test-ccl.42830 "a" 1 nil) "a 1")
           :no-errors))
  :no-errors)


(deftest ccl.bug#305
    (let* ((file (test-source-file "
  (in-package :cl-test)
  (defclass ccl.bug#305-inner () ((ccl.bug#305-inner-slot :accessor ccl.bug#305-inner-slot)))
  (macrolet ((generator ()
               `(defclass ccl.bug#305 (ccl.bug#305-inner)
                  ,(loop for i from 0 to 600
                         for slot = (intern (format nil \"CCL.BUG#305-SLOT-~~A\" i) :cl-user)
                         collect `(,slot :initform ,i)))))
    (generator))
  (defmethod initialize-instance :after ((x ccl.bug#305-inner) &key)
    (setf (ccl.bug#305-inner-slot x) 42))
  (defun ccl.bug#305-test () (make-instance 'ccl.bug#305))"))
           (fasl (test-compile file)))
      (load fasl :verbose nil)
      (ccl.bug#305-inner-slot (ccl.bug#305-test)))
  42)

(deftest ccl.42923
    (progn
      (fmakunbound 'ccl.42923)
      (defmethod ccl.42923 ((x (eql 'x)) &key y &allow-other-keys)
        (list x y) 'x)
      (defmethod ccl.42923 ((x (eql 'foo)) &key y &allow-other-keys)
        (list x y) 'foo)
      (defmethod ccl.42923 ((x (eql 'bar)) &key y z a b c)
        (list x y z (list a b c)) 'bar)
      (ccl::maybe-hack-eql-methods #'ccl.42923)
      (ccl:advise ccl.42923 'advise)
      (ccl.42923 'foo :y 1 :z 2 :a 1 :b 2 :c 3))
  foo)

