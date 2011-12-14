;;;-*-Mode: LISP; Package: CL-TEST -*-
;;;
;;;   Copyright (C) 2008 Clozure Associates

(in-package :cl-test)

(defvar *test-source-file-counter* 0)

(defun test-source-file (format-string &rest format-args)
  (let ((file (format nil "temp~s.dat" (incf *test-source-file-counter*))))
    (with-open-file (s file :direction :output :if-exists :supersede)
      (apply #'format s format-string format-args)
      (terpri s)
      (truename s))))

(defun test-compile (lambda-or-file &rest args &key hide-warnings (safety 1) &allow-other-keys)
  ;; Compile in a more-or-less standard environment
  (let ((*error-output* (if hide-warnings (make-broadcast-stream) *error-output*))
        (ccl::*nx-speed* 1)
        (ccl::*nx-space* 1)
        (ccl::*nx-safety* safety)
        (ccl::*nx-cspeed* 1)
        (ccl::*nx-debug* 1))
    (remf args :hide-warnings)
    (remf args :safety)
    (if (consp lambda-or-file)
      (apply #'compile nil lambda-or-file args)
      (apply #'compile-file lambda-or-file args))))

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
  (progn
    (fmakunbound 'cl-test::ccl.40207-fn)
    ;; Check that these compile-time errors don't abort compilation.
    (let* ((test (test-source-file "(defun cl-test::ccl.40207-fn ()
                                     (and (typep (lambda (x) (setq x)) 'function)
                                          (typep (lambda (x) (setf x)) 'function)
                                          (typep (lambda (((foo))) foo) 'function)
                                          :good))")))
      (test-compile test :hide-warnings t :break-on-program-errors nil :load t)
      (funcall 'cl-test::ccl.40207-fn)))
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
        (test-compile `(lambda (x)
                         (make-array x :element-type ',(gensym))))
      (warning (c)
        (when (typep c 'ccl::compiler-warning)
          (ccl::compiler-warning-warning-type c))))
  :undefined-type)


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
                       :hide-warnings t)
         (test-compile '(lambda ()
                         (ccl:require-type nil '(or ccl.bug#286-unknown-type-2 null)))
                       :hide-warnings t)
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
      (handler-case
          (test-compile file :hide-warnings t :break-on-program-errors nil)
        ;; Might still signal due to macros being implicitly eval-when compile.
        ;; Ok so long as it's not the make-load-form error (which is not a program-error).
        (program-error () nil))
      :no-crash)
  :no-crash)

(deftest ccl.bug#288
    (let ((file (test-source-file "(prog1 (declare (ignore foo)))")))
      (test-compile file :hide-warnings t :break-on-program-errors nil)
      :no-crash)
  :no-crash)

(deftest ccl.bug#288-1 ;; follow-on bug, not really the same
    (let ((file (test-source-file "(defun cl-test::ccl.bug#288-1-fn ((x integer)) x)")))
      (test-compile file :hide-warnings t :break-on-program-errors nil :load t)
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
          (progn (test-compile file :break-on-program-errors nil) :no-warnings)
        (warning (c) c)))
  :no-warnings)


(deftest ccl.40055-3
    (let ((file (test-source-file "
 (defclass ccl.40055-3-class () ())
 (defun ccl.40055-3-cfn () (require-type nil '(or ccl.40055-3-class null)))
 (defstruct ccl.40055-3-struct)
 (defun ccl.40055-3-rfn () (require-type nil '(or ccl.40055-3-struct null)))")))
      (handler-case
          (progn (test-compile file :break-on-program-errors nil) :no-warnings)
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
      (handler-case (progn (test-compile file :break-on-program-errors nil) :no-warnings)
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
      (handler-case (progn (test-compile file :break-on-program-errors nil) :no-warnings)
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

(deftest ccl.bug#252a
    (let ((pn "bug252.dat"))
      (when (probe-file pn)
	(delete-file pn))
      (let ((stream (open pn :direction :output :if-exists :error)))
        (print "something" stream)
        (close stream :abort t)
        (probe-file pn)))
  nil)

(deftest ccl.bug#252b
    (let ((pn "bug252.dat"))
      (when (probe-file pn)
	(delete-file pn))
      (let ((stream (open pn :direction :output)))
        (format stream "something~%")
        (close stream))
      (let ((stream (open pn :direction :output :if-exists :supersede)))
        (format stream "other~%")
        (force-output stream)
        (close stream :abort t))
      (with-open-file (stream pn)
        (let ((line  (read-line stream)))
          (if (equalp line "something") :something line))))
  :something)

(deftest ccl.bug#310
    (remove-duplicates '(1 0 1 1 1 0 0 0 1 0 1 0 1) :end 11)
  (0 1 0 1))

(deftest ccl.bug#294-1
  (handler-case
      (let ((ccl::*nx-safety* 1)) ;; At safety 3, we don't know from EQ...
        (eval '(defun cl-test::ccl.bug#294-1 (x y)
                (eq x) y)))
    (program-error () :program-error))
  :program-error)

(deftest ccl.bug#294-2
  (let* ((file (test-source-file
                "(defun cl-test::ccl.bug#294-2 (x y) (eq x) y)")))
    (fmakunbound ' cl-test::ccl.bug#294-2)
    (handler-case (test-compile file :break-on-program-errors t)
      (program-error () :program-error)))
  :program-error)

(deftest ccl.buf#294-3
  (let* ((file (test-source-file
                "(defun cl-test::ccl.bug#294-3 (x y) (eq x) y)"))
         (warnings nil))
    (fmakunbound ' cl-test::ccl.bug#294-3)
    (list
     (let ((*error-output* (make-broadcast-stream)))
       (handler-case
           (handler-bind ((warning (lambda (c) (setq warnings t))))
             (test-compile file :break-on-program-errors :defer))
         (error (c) :error)))
     warnings))
  (:error t))


(deftest ccl.buf#294-4
  (let* ((file (test-source-file
                "(defun cl-test::ccl.bug#294-4 (x y) (eq x) y)"))
         (warnings nil))
    (fmakunbound 'cl-test::ccl.bug#294-4)
    (list
     (let ((*error-output* (make-broadcast-stream)))
       (handler-bind ((warning (lambda (c) (setq warnings t))))
         (test-compile file :break-on-program-errors nil :load t))
       (handler-case (and (fboundp 'cl-test::ccl.bug#294-4)
                          (funcall 'cl-test::ccl.bug#294-4 1 2))
         (program-error (c) :program-error)))
     warnings))
  (:program-error t))

(deftest ccl.bug#315
    (let* ((file (test-source-file
                  "(defmethod ccl.bug#315-fn ((a sequence))
                       (reduce #'or a :key #'identity))"))
           (warning nil))
      (handler-bind ((warning
                      (lambda (c)
                        (let ((s (princ-to-string c)))
                          (setq warning
                                (if (and (search "FUNCTION" s) (search "macro OR" s))
                                  (or warning :macro-or)
                                  c))))))
        (test-compile file :hide-warnings t :break-on-program-errors nil :load t))
      warning)
  :macro-or)

(deftest ccl.43101a
    (progn
      (untrace)
      (fmakunbound 'ccl.43101a-fun)
      (defun ccl.43101a-fun (x) x)
      (trace ccl.43101a-fun)
      (let ((file (test-source-file "(defun cl-test::ccl.43101a-fun (x) (1+ x))")))
        (test-compile file :hide-warnings t :load t))
      (not (equal "" (with-output-to-string (*trace-output*)
                       (assert (eql (ccl.43101a-fun 4) 5))))))
  t)

(deftest ccl.43101b
    (progn
      (untrace)
      (fmakunbound 'ccl.43101b-gf)
      (defmethod ccl.43101b-gf (x) x)
      (trace ccl.43101b-gf)
      (let ((file (test-source-file "(defmethod cl-test::ccl.43101b-gf (x) (1+ x))")))
        (test-compile file :hide-warnings t :load t))
      (not (equal "" (with-output-to-string (*trace-output*)
                       (assert (eql (ccl.43101b-gf 4) 5))))))
  t)



(deftest ccl.file-stream-typep
    (with-open-file (f "temp.dat" :direction :output :if-exists :supersede)
      (funcall (lambda (f) (let ((type (type-of f)))
                             (and (typep f 'file-stream) (subtypep type 'file-stream) t)))
               f))
  t)


(deftest ccl.complex-cos
    (< (imagpart (cos (complex 1 1))) 0)
  t)

(deftest ccl.space-symbol
    (let* ((list '(|aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|
		   | | | | | | | | | | | | | | | | | | | | | |
		   |aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|))
	   (result (read-from-string
		    (with-output-to-string (s)
		      (let ((*print-readably* t))
			(pprint list s))))))
      (or (equal list result) result))
  t)

(deftest ccl.46016
    (let ((file (test-source-file "
  (defvar var.46016 nil)
  (declaim (boolean var.46016))")))
      (handler-case (progn (test-compile file :load t :break-on-program-errors nil) :no-warnings)
        (warning (c) :warning)))
  :no-warnings)


(deftest ccl.47102
    (handler-case
        (progn
          (defclass ccl.47102 () ((slot :allocation :class)))
          ;; This happens as part of snap-reader-methods optimization
          (ccl::optimize-make-instance-for-class-cell (gethash 'ccl.47102 ccl::%find-classes%))
          :no-warnings)
      (warning (c) :warning))
  :no-warnings)
 

(deftest ccl.47762
    (let ((file (test-source-file
                  "(defun ccl.47762 ()
                     (funcall (find-symbol \"TEST.47762a\" \"NO_SUCH_PACKAGE\"))
                     (funcall (intern \"TEST.47762b\" \"NO_SUCH_PACKAGE-1\")))")))
      (handler-case
          (progn (test-compile file :load t) :no-error)
        (error (c) c)))
  :no-error)


(deftest ccl.bug#254
  (let ((warnings nil)
        (test "
(define-method-combination ccl.bug#254 ()
         ((around (:around))
          (before (:before))
          (primary () :required t)
          (after (:after)))
   (:arguments &optional args)

   (flet ((call-methods (methods)
            (mapcar #'(lambda (method)
                        `(call-method ,method))
                    methods)))
     (let ((form (if (or before after (rest primary))
                     `(multiple-value-prog1
                        (progn ,@(call-methods before)
                               (call-method ,(first primary)
                                            ,(rest primary)))
                        ,@(call-methods (reverse after)))
                     `(call-method ,(first primary)))))
        `(progn (print ,args)
       ,(if around
           `(call-method ,(first around)
                         (,@(rest around)
                          (make-method ,form)))
           form)))))
"))
    (handler-bind ((warning (lambda (c)
                              (push c warnings)
                              (muffle-warning c))))
      (test-compile (test-source-file test)))
    warnings)
  ())

(defun test-dup-warnings (test1 &optional test2)
  (let ((warnings nil))
    (handler-bind ((warning (lambda (c)
                              (let ((msg (format nil "~a" c)))
                                (push (if (search "Duplicate" msg :test #'equalp)
                                        :duplicate-definition
                                        c) warnings)
                                (muffle-warning c)))))
      (if test2
        (with-compilation-unit (:override t)
          (test-compile (test-source-file test1) :hide-warnings t)
          (test-compile (test-source-file test2) :hide-warnings t))
        (test-compile (test-source-file test1) :hide-warnings t)))
    warnings))



(deftest ccl.41334-1
    (test-dup-warnings
     "(defun test.ccl-41334-1 (x) x)
      (defun test.ccl-41334-1 (x) x)")
  (:duplicate-definition))


(deftest ccl.41334-2
    (test-dup-warnings
     "(defmethod test.ccl-41334-2 ((x stream)) x)
      (defmethod test.ccl-41334-2 ((x stream)) x)")
  (:duplicate-definition))


(deftest ccl.41334-3
    (test-dup-warnings
     "(defmacro test.ccl-41334-3 (x) x)
      (defmacro test.ccl-41334-3 (x) x)")
  (:duplicate-definition))

(deftest ccl.41334-4
    (test-dup-warnings
     "(defgeneric test.ccl-41334-4 (x))
      (defun test.ccl-41334-4 (x) x)")
  (:duplicate-definition))


(deftest ccl.41334-1a
    (test-dup-warnings
     "(defun test.ccl-41334-1 (x) x)"
     "(defun test.ccl-41334-1 (x) x)")
  (:duplicate-definition))


(deftest ccl.41334-2a
    (test-dup-warnings
     "(defmethod test.ccl-41334-2 ((x stream)) x)"
     "(defmethod test.ccl-41334-2 ((x stream)) x)")
  (:duplicate-definition))


(deftest ccl.41334-3a
    (test-dup-warnings
     "(defmacro test.ccl-41334-3 (x) x)"
     "(defmacro test.ccl-41334-3 (x) x)")
  (:duplicate-definition))

(deftest ccl.41334-4a
    (test-dup-warnings
     "(defgeneric test.ccl-41334-4 (x &key foo))"
     "(defmacro test.ccl-41334-4 (x) x)")
  (:duplicate-definition))


(deftest ccl.41334-5
    (test-dup-warnings
     "(defclass test.41334-5 () ((41334-5-slot :accessor test.41334-5-slot)))"
     "(defmethod (setf test.41334-5-slot) (v (x test.41334-5)) v)")
  (:duplicate-definition))


(deftest ccl.41334-6
    (test-dup-warnings
     "(defun test.41334-6 () nil)"
     "(let ((closed nil))
        (defun test.41334-6 () closed))")
  (:duplicate-definition))

(deftest ccl.41334-7
    (test-dup-warnings
     "(defun test.41334-7 () nil)"
     "(unless (fboundp 'test.31334-7)
        (defun test.41334-7 () t))")
  nil)

(deftest ccl.41334-8
    (test-dup-warnings
     "(defun (setf test.41334-8) (val) val)"
     "(let ((closed nil))
         (defun (setf test.41334-8) (val) val closed))")
  (:duplicate-definition))

(deftest ccl.49321
    (test-dup-warnings
     "(defclass ccl.49321 () ((x :initarg :x)))
      (progn
         (print 'ccl.49321)
         (let ((go (defun make-ccl.49321 (&key x) (make-instance 'ccl.49321 :x x))))
            go))")
  nil)

#+not-yet
(deftest ccl.bug#340
    (labels ((fact (n) (if (zerop n) 1 (* n (fact (1- n))))))
      (let ((res (format nil "~s" (log (fact 1000) 10.0d0))))
        (or (string-equal "2567.60464" res :end2 10) res)))
  t)

(deftest ccl.bug#344
    (flet ((try (count)
             (let ((cname (gensym))
                   (gname (gensym)))
               (eval `(progn
                        (defclass ,cname () ())
                        ,.(loop for n from 1 to count
                                collect `(defmethod ,gname ((arg0 ,cname) (arg1 (eql ,n)))))))
               (handler-case (progn (funcall gname (make-instance cname) 1) nil)
                 (error (c) :error)))))
      (list (try 46) (try 200)))
  (nil nil))


(deftest ccl.50130
    ;; The compiler policy hack is just to have a predicatable way to catch the bug.
    ;; It doesn't have anything to do with causing the bug to happen.
    (let ((ccl::*default-file-compilation-policy* (ccl::new-compiler-policy :declarations-typecheck
                                                                            t))
          (f (test-source-file "(defun cl-test::ccl.50130-fn (arr idx)
                                  (aref (the (or (vector fixnum) (vector (unsigned-byte 8))) arr) idx))")))
      (test-compile f :load t)
      (funcall 'cl-test::ccl.50130-fn (make-array 4 :element-type 'fixnum :initial-element 17) 2))
  17)

(deftest ccl.50646-bug#378
    (progn
      (define-method-combination ccl.50646-method-combination ()
        ((around (:around)) (primary ()))
        `(call-method ,(first around) ((make-method (call-method ,(first primary))))))
      (defgeneric ccl.50646-gf (x) (:method-combination ccl.50646-method-combination))
      (defmethod ccl.50646-gf ((x integer)) x)
      (defmethod ccl.50646-gf :around ((x integer)) (call-next-method x))
      (ccl.50646-gf 23))
  23)

(deftest ccl.50911
    (progn
      (defclass ccl.50911-class () ((slot-a :initarg :a :reader ccl.50911-slot-a)))
      (ccl::%snap-reader-method #'ccl.50911-slot-a)
      (ccl:finalize-inheritance (find-class 'ccl.50911-class))
      (ccl.50911-slot-a (make-instance 'ccl.50911-class :a :test)))
  :test)

(deftest ccl.50911-a
    (let ((called 0))
      (defclass ccl.50911-a () ())
      (defun ccl.50911-a-fn () (make-instance 'ccl.50911-a))
      (defmethod initialize-instance ((x ccl.50911-a) &rest keys) keys (incf called))
      (ccl.50911-a-fn)
      (defmethod initialize-instance :after ((x ccl.50911-a) &rest keys) keys (incf called))
      (ccl.50911-a-fn)
      (ccl::optimize-make-instance-for-class-name 'ccl.50911-a)
      (ccl.50911-a-fn)
      called)
  5)


(deftest ccl.bug-misc-init
    (progn
      (funcall (lambda () (make-array 1 :element-type '(signed-byte 16) :initial-element -1)))
      t)
  t)
  
(deftest ccl.bug#382
    (string= (with-output-to-string (s)
	       (funcall #'(lambda () (write-string "foobar" s :end 2))))
	     "fo")
  t)
  
(deftest ccl.52006
    (progn
      (defclass ccl.52006-class () ((slot :initarg :slot)) (:default-initargs :slot nil))
      (defun test-1 (args) (apply #'make-instance 'ccl.52006-class args))
      (ccl::optimize-make-instance-for-class-name 'ccl.52006-class)
      (slot-value (test-1 nil) 'slot))
  nil)


(deftest ccl.bug#387
    (handler-case
        (coerce #(127 0 0 256) '(simple-array (unsigned-byte 8) (*)))
      (type-error () :type-error))
  :type-error)

(deftest ccl.49462
    (let ((file (test-source-file "(defun ccl.49462-fn (x) x)
(defmacro ccl.49462-macro (x) (error \"(macro ~~s)\" x))
(ccl.49462-macro 1)")))
      (handler-case
          (with-compilation-unit (:override t)
            (handler-bind ((error (lambda (c)
                                    (declare (ignore c))
                                    (with-open-file (f file :direction :output)
                                      (format f "(defun ccl.49462-fn (x) x)"))
                                    (invoke-restart 'ccl::retry-compile-file))))
              (test-compile file :hide-warnings t))
            nil)
        (warning (c) c)))
  nil)

(deftest ccl.49462-redux-1
    (let ((file (test-source-file "(defun ccl.49462-redux-1-fn (x) x)")))
      (handler-case 
          (with-compilation-unit (:override t)
            (test-compile file :hide-warnings t)
            (test-compile file :hide-warnings t)
            nil)
        (warning (c) c)))
  nil)


(deftest ccl.49462-redux-2
    (let ((file (test-source-file "(defun ccl.49462-redux-2-fn (x) x)"))
          (warnings ()))
      (handler-bind ((warning (lambda (c) (push c warnings))))
        (with-compilation-unit (:override t)
          (with-compilation-unit ()
            (test-compile file))
          (test-compile file :hide-warnings t)))
      (length warnings))
  1)


(deftest ccl.bug-overflow-handling
    (funcall (test-compile '(lambda ()
                             (let ((upper-bound most-positive-fixnum))
                               (let ((lower-bound (- (1+ upper-bound))))
                                 lower-bound)))))
  #.most-negative-fixnum)


(deftest ccl.bug#412
    (funcall (test-compile '(lambda ()
                             (let* ((x most-negative-fixnum)
                                    (y 1))
                               (- x y)))))
  #.(1- most-negative-fixnum))

(deftest ccl.bug#411
    (funcall (test-compile '(lambda ()
                             (let ((x 0)) (+ 3416133997 x)))))
  3416133997)

(deftest ccl.51790
    (let ((var))
      (setq var t)
      (list
       (handler-case (format nil "~:[First case;second case~]" var)
         (error () :error))
       (handler-case (format nil "~:[First case;second case~]" (not var))
         (error () :error))))
  (:error :error))

(deftest ccl.bug#409
    (let ((errors ()))
      (handler-bind ((ccl::compiler-warning
                      (lambda (c)
                        (push (ccl::compiler-warning-function-name c) errors)
                        (muffle-warning c))))
        (let ((file (test-source-file "(in-package :cl-test)
                                       (defun ccl.bug#409a1 (x) (declare (type 17 x)) x)
                                       (defun ccl.bug#409a2 (x) x (the 17 x))
                                       (defun ccl.bug#409a3 (x) x (typep x 17))
                                       (defun ccl.bug#409a4 (x) x (make-array 3 :element-type 17))

                                       (defun ccl.bug#409b1 (x) (declare (type (cons number number list) x)) x)
                                       (defun ccl.bug#409b2 (x) x (the (cons number number list) x))
                                       (defun ccl.bug#409b3 (x) x (typep x '(cons number number list)))
                                       (defun ccl.bug#409b4 (x) x (make-array 3 :element-type '(cons number number list)))

                                       (defun ccl.bug#409c1 (x) (declare (type (sequence symbol) x)) x)
                                       (defun ccl.bug#409c2 (x) x (the (sequence symbol) x))
                                       (defun ccl.bug#409c3 (x) x (typep x '(sequence symbol)))
                                       (defun ccl.bug#409c4 (x) x (make-array 3 :element-type '(sequence symbol) :initial-element x))
                                      ")))
          (test-compile file :hide-warnings t :break-on-program-errors nil)))
      errors)
  ((ccl.bug#409c4) (ccl.bug#409c3) (ccl.bug#409c2) (ccl.bug#409c1)
   (ccl.bug#409b4) (ccl.bug#409b3) (ccl.bug#409b2) (ccl.bug#409b1)
   (ccl.bug#409a4) (ccl.bug#409a3) (ccl.bug#409a2) (ccl.bug#409a1)))

(deftest ccl.53584
    (let ((file (test-source-file "(defclass cl-test::ccl.53584 () ((x :type (sequence integer) :initarg :x)))"))
          (warnings ()))
      (handler-case
          (handler-bind ((ccl::compiler-warning
                          (lambda (c) (push :compile-time warnings) (muffle-warning c)))
                         (warning
                          (lambda (c) (push :load-time warnings) (muffle-warning c))))
            (test-compile file :hide-warnings t :load t)
            (make-instance 'ccl.53584 :x '(17)))
        (error () (push :run-time warnings)  warnings)))
  (:run-time :load-time :compile-time))

(deftest ccl.bug#321
    (handler-case
        (progn
          (format nil "~a" (make-condition 'style-warning))
          :no-error)
      (error () :error))
  :no-error)

(deftest ccl.loop-array
    (let ((x nil))
      (declare (optimize (safety 3) (speed 1)))
      (setq x nil)
      (handler-case
          (loop for a across x collect a)
        (type-error () :error)))
  :error)

(deftest ccl.loop-on
    (locally (declare (optimize (safety 3) (speed 1)))
      (loop for (head . tail) on '(a . b) when head collect tail))
  (b))


;;; This is likely to return random nonsense (without necessarily
;;; getting a memory fault) on some platforms.
#+bogus-test
(deftest ccl.57900.1 ;; don't crash on simple access errors
    (handler-case (funcall (lambda (x) (declare (optimize (safety 1) (speed 1))) (ccl::%caar x))
                           *standard-input*)
      (storage-condition () :storage-condition))
  :storage-condition)

(deftest ccl.57900.2
    (handler-case (funcall (lambda (x) (declare (optimize (safety 1) (speed 1))) (ccl::%caar x))
                           0)
      (storage-condition () :storage-condition))
  :storage-condition)

(deftest ccl.next-method-p
    (let ((file (test-source-file "(defmethod cl-test::ccl.next-method-gf (x) (if (next-method-p) (call-next-method) x))")))
      (fmakunbound 'cl-test::ccl.next-method-gf)
      (test-compile file :load t)
      (funcall 'cl-test::ccl.next-method-gf 3))
  3)

(deftest ccl.49345-1
    (test-dup-warnings
     "(defclass test.ccl-49345-1 () ())
      (defclass test.ccl-49345-1 () ())")
  (:duplicate-definition))

(deftest ccl.49345-2
    (test-dup-warnings
     "(defstruct (test.ccl-49345-2 (:copier  nil) (:predicate nil) (:constructor nil)))
      (defstruct (test.ccl-49345-2 (:copier  nil) (:predicate nil) (:constructor nil)))")
  (:duplicate-definition))

(deftest ccl.49345-3
    (test-dup-warnings
     "(deftype test.ccl-49345-3 () 'integer)
      (deftype test.ccl-49345-3 () 'integer)")
  (:duplicate-definition))

(deftest ccl.49345-4
    (test-dup-warnings
     "(defclass test.ccl-49345-4 () ())
      (deftype test.ccl-49345-4 () 'integer)")
  (:duplicate-definition))

#+not-yet
(deftest ccl.49345-5
    (test-dup-warnings
     "(defclass test.ccl-49345-5 () ())
      (let ((closed nil))
         (defclass test.ccl-49345-5 () ((slot :initform closed))))")
  (:duplicate-definition))

#+not-yet
(deftest ccl.49345-6
    (test-dup-warnings
     "(defclass test.ccl-49345-6 () ())"
     "(let ((closed nil))
         (defstruct test.ccl-49345-6 (x closed)))")
  (:duplicate-definition))

(deftest ccl.49345-7
    (test-dup-warnings
     "(defclass test.ccl-49345-7 () ())
      (when (find-class 'test.ccl-49345-7 nil)
         (defclass test.ccl-49345-7 () ()))")
  ())

(defun test-compiler-warning (text &key (safety 1))
  (let ((warnings nil))
    (handler-bind ((ccl::compiler-warning (lambda (c)
					    (push (ccl::compiler-warning-warning-type c) warnings)
					    (muffle-warning c))))
      (test-compile (test-source-file "~a" text) :hide-warnings t :break-on-program-errors nil :safety safety))
    (nreverse warnings)))
  
(deftest ccl.49345-u1
    (test-compiler-warning "(defun ccl.49345-u1 (x) (typep x 'ccl.49345-u1-type))")
  (:undefined-type))

(deftest ccl.49345-u2
    (test-compiler-warning "(defun ccl.49345-u2 (x) (declare (type ccl.49345-u2-type x)) x)")
  (:unknown-type-in-declaration))

(deftest ccl.49345-u3
    (test-compiler-warning "(defun ccl.49345-u3 (x) (the ccl.49345-u3-type x))")
  (:unknown-type-in-declaration))

(deftest ccl.49345-u4
    (test-compiler-warning "(defun ccl.49345-u4 (x) (make-array x :element-type 'ccl.49345-u4-type))")
  (:undefined-type))

(deftest ccl.49345-u5
    (test-compiler-warning "(defun ccl.49345-u5 (x) (coerce x 'ccl.49345-u5-type))")
  (:undefined-type))

(deftest ccl.49345-u6
    (test-compiler-warning "(declaim (type ccl.49345-u6-type *ccl.49345-u6*))")
  (:undefined-type))

(deftest ccl.49345-i1
    (test-compiler-warning "(defun ccl.49345-i1 (x) (typep x '(sequence integer)))")
  (:invalid-type))

(deftest ccl.49345-i2
    (test-compiler-warning "(defun ccl.49345-i2 (x) (declare (type (sequence integer) x)) x)")
  (:invalid-type))

(deftest ccl.49345-i3
    (test-compiler-warning "(defun ccl.49345-i3 (x) (the (sequence integer) x))")
  (:invalid-type))

(deftest ccl.49345-i4
    (test-compiler-warning "(defun ccl.49345-i4 (x) (make-array x :element-type '(sequence integer)))")
  (:invalid-type))

(deftest ccl.49345-i5
    (test-compiler-warning "(defun ccl.49345-i5 (x) (coerce x '(sequence integer)))")
  (:invalid-type))

(deftest ccl.49345-i6
    (test-compiler-warning "(declaim (type (sequence integer) *ccl.49345-i6*))")
  (:invalid-type))

(deftest ccl.49345-fwd
    (test-compiler-warning "(defun ccl.49345-fwd-fn (x ) (typep x 'ccl.49345-fwd-type))
                            (defclass ccl.49345-fwd-type () ())")
  ())

(deftest ccl.57879-1
    (test-compiler-warning "(defun foo (x) (declare (ccl.57879-1 'foo)) x)")
  (:bad-declaration))

(deftest ccl.57879-2
    (handler-case
        (test-compile (test-source-file "(proclaim '(ccl.57879-2 3))") :hide-warnings t :load t)
      (program-error () :error))
  :error)

(deftest ccl.57879-3
    (test-compiler-warning "(declaim (ccl.57879-3 3))")
  (:bad-declaration))

(deftest ccl.57879-4
    (handler-case
        (test-compile (test-source-file "(proclaim '(optimize (ccl.57879-4a ccl.57879-4b)))") :hide-warnings t :load t)
      (program-error () :error))
  :error)

(deftest ccl.57879-5
    (test-compiler-warning "(declaim (optimize (ccl.57879-5a ccl.57879-5b)))")
  (:bad-declaration))

;; By special dispensation, don't complain, even though can't optimize the slot reference.
(deftest ccl.57879-6
    (test-compiler-warning "(defstruct ccl.57879-6-struct (slot nil :type (or null ccl.57879-6-type)))
                            (defun ccl.57879-6-fn (x) (ccl.57879-6-struct-slot x))

                            (deftype ccl.57879-6-type () 'null)")
  ())

;; Same as above, but at safety 3.
(deftest ccl.86893
    (test-compiler-warning "(defstruct ccl.86893-struct (slot nil :type (or null ccl.86893-type)))
                            (defun ccl.86893-fn (x) (ccl.86893-struct-slot x))

                            (deftype ccl.86893-type () 'null)"
                           :safety 3)
  ())

(deftest ccl.sbcl-bootstrap-1 ;; For sbcl bootstrap, undefined type needs to be a style warning.
    (multiple-value-bind (truename warnings-p serious-p)
	(test-compile (test-source-file "(defun ccl.sbcl-bootstrap-1a (x)
                                           (declare (type unknown-type-ccl.sbcl-bootstrap-1a x))
                                           x)")
		      :hide-warnings t)
      (declare (ignore truename))
      (list warnings-p serious-p))
    (t nil))


(deftest ccl.59726
    (test-compiler-warning "(defun ccl.59726-fn () #'ccl.59726-unknown)")
  (:undefined-function))

(deftest ccl.bug#470
    (funcall (lambda ()
               (declare (optimize (safety 1) (speed 1)))
               (let ((array (make-array '(1 1) :initial-element 2.0
                                        :element-type 'single-float))
                     (var 1.0))
                 (setf (aref array 0 0) var
                       var nil))))
  nil)

(deftest ccl.55959.bug#474
    (block test
      (handler-bind ((program-error (lambda (c)
                                      (declare (ignore c))
                                      (return-from test
                                        (handler-case (progn
                                                        (with-output-to-string (s)
                                                          (ccl:print-call-history :stream s))
                                                        :success)
                                          (error (c) c))))))
        (labels ((inner (x &key a)
                   ;; try to make sure this will use at least one saved register
                   (loop (concatenate x a) (concatenate x a) (concatenate x a)))
                 (outer (x)
                   ;; try to make sure this will use a saved register for X so backtrace will try to find it.
                   (setq x (list (list x) :bogus-key (list (list x) (list x))))
                   ;; call inner with bad keyword arg, to cause error before it saves its saved regs
                   (apply #'inner x)
                   x))
          (declare (notinline inner outer))
          (outer 3))))
  :success)

(deftest ccl.r12217
    (with-input-from-string (s "123")
      (file-position s 3))
  3)

(deftest ccl.the-with-constant-values
  (eval '(the (values integer) 23))
  23)

(defmacro ccl.bug#543.macro (init) `(make-array (length ,init)))

(deftest ccl.bug#543
    (length (funcall (lambda () (progn (the array (ccl.bug#543.macro '(a b)))))))
  2)

(deftest ccl.bug#543a
    (handler-case
        (progn
          (test-compile '(lambda (x y)
                          (the fixnum (- (the fixnum (aref (the (array fixnum 1) x) (aref (the (simple-array fixnum 1) y) 0)))))))
          :win)
      (serious-condition (c) c))
  :win)

(deftest ccl.r12429
    (let ((ccl::*print-string-length* 10))
      (with-standard-io-syntax
	  (values (read-from-string (prin1-to-string "123456789012345")))))
  "123456789012345")

(deftest ccl.63842a
    (test-compiler-warning "(defun ccl.63842a-1 () (declare (inline ccl.63842a-2)))")
  (:unknown-declaration-function))

(deftest ccl.63842b
    (test-compiler-warning "(defun ccl.63842b-1 () (declare (dynamic-extent #'ccl.63842b-2)))")
  (:unknown-declaration-function))

(deftest ccl.decl.1
    (test-compiler-warning "(defun ccl.decl.1 (a) (lambda () (declare (fixnum a)) a))")
  ())

(deftest ccl.decl.2
    (test-compiler-warning "(defun ccl.decl.2 (a) (flet ((fn () (declare (fixnum a)) a)) #'fn))")
  ())

(deftest ccl.decl.3
    (test-compiler-warning "(defun ccl.decl.3 ()
                              (declare (dynamic-extent #'ccl.decl.3-none-such)
                                       (notinline ccl.decl.3-none-other)))")
  (:unknown-declaration-function :unknown-declaration-function))

(deftest ccl.decl.4
    (test-compiler-warning "(defun ccl.decl.4 () (flet ((fn () t) (fn1 () t)) (declare (inline fn) (dynamic-extent #'fn1)) (list (fn) (fn1))))")
  ())

(deftest ccl.decl.5
    (test-compiler-warning "(defun ccl.decl.5 () (flet ((fn () t)) (declare (notinline ccl.decl.5-none-sch) (dynamic-extent #'ccl.decl.5-non-other)) #'fn))")
  (:unknown-declaration-function :unknown-declaration-function))

(deftest ccl.ftype.1
    (test-compiler-warning "(lambda () (declare (ftype integer ccl.ftype.1)))")
  (:bad-declaration))

(deftest ccl.ftype.2
    (test-compiler-warning "(lambda () (declare (ftype function ccl.ftype.2)) #'ccl.ftype.2)")
  ())

(deftest ccl.ftype.3
    (test-compiler-warning "(declaim (ftype (function (t) (values integer)) ccl.ftype.3))
                            (defun ccl.ftype.3-caller () (the cons (ccl.ftype.3 nil)))")
  (:type-conflict))


(deftest ccl.ftype.4
    (test-compiler-warning "(declaim (ftype (function (t) (values integer)) ccl.ftype.4))
                            (defun ccl.ftype.4-caller () (ccl.ftype.4))")
  (:ftype-mismatch))

(deftest ccl.ftype.5
    (test-compiler-warning "(declaim (ftype (function (t &key (:a integer)) (values integer)) ccl.ftype.5))
                            (defun ccl.ftype.5-caller () (ccl.ftype.5 1 :a :x))")
  (:type))

(deftest ccl.ftype.6
    (test-compiler-warning "(declaim (ftype (function (t &key (:a integer)) (values integer)) ccl.ftype.6))
                            (defun ccl.ftype.6-caller () (ccl.ftype.6 :b 17))")
  (:ftype-mismatch))


(deftest ccl.ftype.7
    (test-compiler-warning "(declaim (ftype (function (t t t) t) ccl.ftype.7))
                            (defun ccl.ftype.7-caller () (ccl.ftype.7))")
  (:ftype-mismatch))

(deftest ccl.ftype.8
    (test-compiler-warning "(declaim (ftype (function (t t t) t) ccl.ftype.8))
                            (defun ccl.ftype.8-caller ()
                               (flet ((ccl.ftype.8 () t)) (ccl.ftype.8)))")
  ())

(deftest ccl.ftype.9-pre
    (test-compiler-warning "(declaim (ftype (function (unknown) t) ccl.ftype.9-pre))")
  (:undefined-type))

(deftest ccl.ftype.9
    (test-compiler-warning "(defun ccl.ftype.9 (x) x)
                            (declaim (ftype (function (unknown) t) ccl.ftype.9))
                            (defun ccl.ftype.9-caller () (ccl.ftype.9 17))")
  ;; The :undefined-type is from the declaim itself (see ccl.ftype.9-pre).  There
  ;; should be no added type warnings from the actual use of the fn
  (:undefined-type))

(deftest ccl.ftype.10
    (test-compiler-warning "(defun ccl.ftype.10-caller (x)
                              (declare (ftype (function (t) t) ccl.ftype.10))
                              (ccl.ftype.10 x))")
  ())


(deftest ccl.ftype.11-pre
    (test-compiler-warning "(defun ccl.ftype.11-pre-caller (x)
                              (declare (ftype (function (unknown) t) ccl.ftype.11-pre))
                              x)")
  (:unknown-type-in-declaration))

(deftest ccl.ftype.11
    (test-compiler-warning "(defun ccl.ftype.11-caller (x)
                              (declare (ftype (function (unknown) t) ccl.ftype.11))
                              (ccl.ftype.11 x))")
  ;; The :unknown-type-in-declaration is from the declare itself (see ccl.ftype.11-pre).  There
  ;; should be no added type warnings from the actual use of the fn
  (:unknown-type-in-declaration :undefined-function))

(deftest ccl.ftype.54161
  (test-compiler-warning "(declaim (ftype (function (integer) (values integer)) ccl.ftype.54161))
  (defun ccl.ftype.54161-caller () (ccl.ftype.54161 :x))")
  (:type))


(deftest ccl.macroexpand-all.r12550a
  (ccl:macroexpand-all '(macrolet ((foo () 'macro)) (flet ((foo () (foo))) (foo))))
  (progn (flet ((foo () macro)) (foo))))

(deftest ccl.macroexpand-all.r12550b
  (ccl:macroexpand-all '(macrolet ((foo () 'macro)) (labels ((foo () (foo))) (foo))))
  (progn (labels ((foo () (foo))) (foo))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ADVISE

(defun function-to-advise (x) (car x))
(defun another-function-to-advise (x) (cdr x))
(defun (setf function-to-advise) (val arg) (setf (car arg) val))

(declaim (notinline function-to-advise
                    another-function-to-advise
                    (setf function-to-advise)))

(defvar *advise-var* nil)


(deftest advise.1
  (progn
    (ccl:unadvise t)
    (function-to-advise '(a)))
  a)

(deftest advise.2
  (progn
    (ccl:unadvise t)
    (ccl:advise function-to-advise (return 'advise.2))
    (function-to-advise '(b)))
  advise.2)

(deftest advise.3
  (progn
    (ccl:unadvise t)
    (ccl:advise function-to-advise 'advised.3 :when :around :name test)
    (assert (eq 'advised.3 (function-to-advise '(a))))
    (prog1 (ccl:advisedp t)
      (ccl:unadvise t)
      (assert (null (ccl:advisedp t)))))
  ((function-to-advise :around test)))


(deftest advise.4
  (progn
    (ccl:unadvise t)
    (ccl:advise function-to-advise (return 'advise.4) :name test)
    (handler-bind ((warning #'muffle-warning))
      (ccl:advise function-to-advise (return 'readvised) :name test))
    (prog1 (ccl:advisedp t)
      (ccl:unadvise t)
      (assert (null (ccl:advisedp t)))))
  ((function-to-advise :before test)))

(deftest advise.4a
  (progn
    (ccl:unadvise t)
    (setq *advise-var* '(none))
    (ccl:advise function-to-advise (push 'advise.4a *advise-var*) :name test)
    (handler-bind ((warning #'muffle-warning))
      (ccl:advise function-to-advise (push 'readvise.4a *advise-var*) :name test))
    (assert (eq (function-to-advise '(c)) 'c))
    *advise-var*)
  (readvise.4a none))

(deftest advise.5
  (progn
    (ccl:unadvise t)
    (setq *advise-var* '(none))
    (ccl:advise (setf function-to-advise) (push 'advise.5 *advise-var*))
    (prog1 (ccl:advisedp t)
      (ccl:unadvise t)
      (assert (null (ccl:advisedp t)))))
  (((setf function-to-advise) :before nil)))

(deftest advise.6
  (progn
    (ccl:unadvise t)
    (setq *advise-var* '(none))
    (ccl:advise (setf function-to-advise) (push 'advise.6 *advise-var*))
    (handler-bind ((warning #'muffle-warning))
      (ccl:advise (setf function-to-advise) (push 'readvise.6 *advise-var*)))
    (prog1 (ccl:advisedp t)
      (ccl:unadvise t)
      (assert (null (ccl:advisedp t)))))
  (((setf function-to-advise) :before nil)))

(deftest advise.6a
  (progn
    (ccl:unadvise t)
    (setq *advise-var* '(none))
    (ccl:advise (setf function-to-advise) (push 'advise.6a *advise-var*) :when :after)
    (handler-bind ((warning #'muffle-warning))
      (ccl:advise (setf function-to-advise) (push 'readvise.6a *advise-var*) :when :after))
    (let ((x (list nil)))
      (list* (setf (function-to-advise x) 17)
             (car x)
             *advise-var*)))
  (17 17 readvise.6a none))

(deftest advise.7
  (progn
    (ccl:unadvise t)
    (setq *advise-var* '(none))
    (let ((x (list nil)))
      (assert (eql (setf (function-to-advise x) 'a) 'a))
      (assert (equal x '(a)))
      *advise-var*))
  (none))

(deftest advise.8
  (progn
    (ccl:unadvise t)
    (setq *advise-var* '(none))
    (ccl:advise (setf function-to-advise) (push 'advise.8 *advise-var*))
    (let ((x (list nil)))
      (assert (eql (setf (function-to-advise x) 'a) 'a))
      (assert (equal x '(a)))
      *advise-var*))
  (advise.8 none))

(deftest advise.9
  (progn
    (ccl:unadvise t)
    (setq *advise-var* '(none))
    (ccl:advise function-to-advise (push 'advise.9 *advise-var*))
    (ccl:advise another-function-to-advise (push 'another-advise.9 *advise-var*))
    (assert (eql (function-to-advise '(b)) 'b))
    (assert (eql (another-function-to-advise '(c . d)) 'd))
    (assert (equal *advise-var* '(another-advise.9 advise.9 none)))
    (prog1
	(sort (copy-list (ccl:advisedp t))
              #'(lambda (k1 k2) (string< (princ-to-string k1)
                                         (princ-to-string k2))))
      (ccl:unadvise t)))
  ((another-function-to-advise :before nil) (function-to-advise :before nil)))

(deftest advise.10
  (progn
    (ccl:unadvise t)
    (setq *advise-var* '(none))
    (assert (null (ccl:advisedp t)))
    (ccl:advise function-to-advise (push 'advise.10 *advise-var*))
    (ccl:unadvise function-to-advise)
    (assert (null (ccl:advisedp t)))
    (handler-bind ((warning #'muffle-warning)) (ccl:unadvise function-to-advise))
    (assert (null (ccl:advisedp t)))
    nil)
  nil)

(deftest advise.11
  (progn
    (ccl:unadvise t)
    (ccl:advise function-to-advise  (return 17))
    (ccl:advise another-function-to-advise (return 18))
    (ccl:unadvise function-to-advise)
    (ccl:unadvise another-function-to-advise)
    (ccl:advisedp t))
  nil)

;;; advising a generic function

(declaim (notinline generic-function-to-advise))

(deftest advise.12
  (progn
    (ccl:unadvise t)
    (setq *advise-var* '(none))
    (eval '(defgeneric generic-function-to-advise (x y)))
    (ccl:advise generic-function-to-advise (push 'advise.12 *advise-var*))
    (prog1 (ccl:advisedp t) (ccl:unadvise t)))
  ((generic-function-to-advise :before nil)))

(deftest advise.13
  (progn
    (ccl:unadvise t)
    (setq *advise-var* '(none))
    (eval '(defgeneric generic-function-to-advise (x y)))
    (ccl:advise generic-function-to-advise (push 'advise.13 *advise-var*))
    (eval '(defmethod generic-function-to-advise ((x t)(y t)) nil))
    (prog1 (ccl:advisedp t) (ccl:unadvise t)))
  ((generic-function-to-advise :before nil)))

(deftest advise.14
  (progn
    (ccl:unadvise t)
    (setq *advise-var* '(none))
    (eval '(defgeneric generic-function-to-advise (x y)))
    (ccl:advise generic-function-to-advise (push 'advise.14 *advise-var*))
    (eval '(defmethod generic-function-to-advise ((x t)(y t)) nil))
    (assert (null (generic-function-to-advise 'a 'b)))
    (assert (equal *advise-var* '(advise.14 none)))
    (prog1
	(ccl:advisedp t)
      (ccl:unadvise generic-function-to-advise)
      (assert (null (ccl:advisedp t)))))
  ((generic-function-to-advise :before nil)))

(declaim (notinline generic-function-to-advise2))

(deftest advise.15
  (progn
    (ccl:unadvise t)
    (setq *advise-var* '(none))
    (let* ((gf (eval '(defgeneric generic-function-to-advise2 (x y))))
	   (m (eval '(defmethod generic-function-to-advise2
		       ((x integer)(y integer))
		       :foo))))
      (eval '(defmethod generic-function-to-advise2
	       ((x symbol)(y symbol)) :bar))
      (assert (eql (generic-function-to-advise2 1 2) :foo))
      (assert (eql (generic-function-to-advise2 'a 'b) :bar))
      (ccl:advise generic-function-to-advise2 (push 'advise.15 *advise-var*))
      (assert (equal (ccl:advisedp t) '((generic-function-to-advise2 :before nil))))
      (remove-method gf m)
      (prog1 (ccl:advisedp t) (ccl:unadvise t))))
  ((generic-function-to-advise2 :before nil)))


(deftest advise.16
  (progn
    (ccl:unadvise t)
    (setq *advise-var* '(none))
    (ccl:advise function-to-advise (push 'advise.16-1 *advise-var*) :name test-1)
    (ccl:advise function-to-advise (push 'advise.16-2 *advise-var*) :name test-2)
    (prog1 (cons (function-to-advise '(foo)) *advise-var*) (ccl:unadvise t)))
  (foo advise.16-1 advise.16-2 none))

(deftest advise.17
  (progn
    (ccl:unadvise t)
    (setq *advise-var* '(none))
    (untrace)
    (ccl:advise function-to-advise (push 'advise.17-1 *advise-var*) :name test-1)
    (trace function-to-advise)
    (ccl:advise function-to-advise (push 'advise.17-2 *advise-var*) :name test-2)
    (prog1
        (list (not (equal "" (with-output-to-string (*trace-output*)
                               (function-to-advise '(foo)))))
              *advise-var*
              (ccl:unadvise function-to-advise :name test-1)
              (not (equal "" (with-output-to-string (*trace-output*)
                               (function-to-advise '(bar)))))
              *advise-var*
              (untrace)
              (with-output-to-string (*trace-output*)
                (function-to-advise '(bar)))
              *advise-var*)
      (ccl:unadvise t)
      (untrace)))
  (t (advise.17-1 advise.17-2 none) ((function-to-advise :before test-1))
     t (advise.17-2 advise.17-1 advise.17-2 none) (function-to-advise) "" 
     (advise.17-2 advise.17-2 advise.17-1 advise.17-2 none)))


(deftest advise.18
  (progn
    (ccl:unadvise t)
    (setq *advise-var* '(none))
    (untrace)
    (fmakunbound 'generic-function-to-advise.18)
    (eval '(defgeneric generic-function-to-advise.18 (x y)))
    (eval '(defmethod generic-function-to-advise.18 ((x integer)(y integer)) :foo))
    (eval '(defmethod generic-function-to-advise.18 ((x symbol)(y symbol)) :bar))
    (ccl:advise generic-function-to-advise.18 (push 'advise.18-1 *advise-var*) :name test-1)
    (trace generic-function-to-advise.18)
    (ccl:advise generic-function-to-advise.18 (push 'advise.18-2 *advise-var*) :name test-2)
    (prog1
        (list (not (equal "" (with-output-to-string (*trace-output*)
                               (assert (eq :bar (generic-function-to-advise.18 'a 'b))))))
              *advise-var*
              (ccl:unadvise generic-function-to-advise.18 :name test-1)
              (not (equal "" (with-output-to-string (*trace-output*)
                               (assert (eq :foo (generic-function-to-advise.18 1 2))))))
              *advise-var*
              (untrace)
              (with-output-to-string (*trace-output*)
                (generic-function-to-advise.18 'x 'y))
              *advise-var*)
      (ccl:unadvise t)
      (untrace)))
  (t (advise.18-1 advise.18-2 none) ((generic-function-to-advise.18 :before test-1))
     t (advise.18-2 advise.18-1 advise.18-2 none) (generic-function-to-advise.18) "" 
     (advise.18-2 advise.18-2 advise.18-1 advise.18-2 none)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest ccl.56248a
    (test-compiler-warning "(defmacro ccl.56248a (&whole whole) t)")
  (:unused))

(deftest ccl.56248b
    (test-compiler-warning "(defmacro ccl.56248b (&environment env) t)")
  (:unused))


(deftest ccl.ctype-hashing
    (let ((path #P"x"))
      (and (not (typep path '(member #P"x")))
           (typep path `(member ,path))
           t))
  t)


(deftest ccl.61783-1
    (test-compiler-warning "(defgeneric ccl.61783-1 (x y))
                            (defmethod ccl.61783-1 ((x integer)) x)")
  (:incongruent-method-lambda-list))

(deftest ccl.61783-1-rev
    (test-compiler-warning "(defmethod ccl.61783-1-rev ((x integer)) x)
                            (defgeneric ccl.61783-1-rev (x y))")
  (:incongruent-gf-lambda-list))


(deftest ccl.61783-2
    (test-compiler-warning "(defmethod ccl.61783-2 ((x integer)) x)
                            (defmethod ccl.61783-2 ((x string) &key) x)")
  (:incongruent-method-lambda-list))

(deftest ccl.61783-3
    (test-compiler-warning "(defgeneric ccl.61783-3 (&key a b))
                            (defmethod ccl.61783-3 (&key a) a)")
  (:gf-keys-not-accepted))

(deftest ccl.61783-3-rev
    (test-compiler-warning "(defmethod ccl.61783-3-rev (&key a) a)
                            (defgeneric ccl.61783-3-rev (&key a b))")
  (:gf-keys-not-accepted))

(deftest ccl.61783-4
    (test-compiler-warning "(defgeneric ccl.61783-4 (&key a))
                            (defgeneric ccl.61783-4 (&key a))")
  (:duplicate-definition))

(deftest ccl.61783-5
    (test-compiler-warning "(defmethod ccl.61783-5 ((x integer) &key a) a)
                            (defun ccl.61783-5-caller () (ccl.61783-5 1 :a 12 :b 0))")
  (:environment-mismatch))

(deftest ccl.61783-5-rev
    (test-compiler-warning "(defun ccl.61783-5-rev-caller () (ccl.61783-5-rev 1 :a 12 :b 0))
                            (defmethod ccl.61783-5-rev ((x integer) &key a) a)")
  (:environment-mismatch))


(deftest ccl.61783-6
    (test-compiler-warning "(defgeneric ccl.61783-6 (x &key a &allow-other-keys))
                            (defun ccl.61783-6-caller () (ccl.61783-6 1 :a 12 :b 0))")
  ())

(deftest ccl.61783-6-rev
    (test-compiler-warning "(defun ccl.61783-6-rev-caller () (ccl.61783-6-rev 1 :a 12 :b 0))
                            (defgeneric ccl.61783-6-rev (x &key a &allow-other-keys))")
  ())


(deftest ccl.61783-7
    (test-compiler-warning "(defgeneric ccl.61783-7 (x &key a &allow-other-keys))
                            (defmethod ccl.61783-7 ((x integer) &rest args) args)")
  ())

(deftest ccl.bug#592
    (test-compiler-warning "(macrolet ((tag () 1))
                              (eval-when (:compile-toplevel :load-toplevel :execute)
                                (assert (= 1 (tag)))))")
  ())

(deftest ccl.bug#601
    (flet ((dispatch-macro-char-p (char &optional (rt *readtable*))
             (handler-case
                 (prog1 t
                   (get-dispatch-macro-character char #\x rt))
               (error () nil))))
      (let ((*readtable* (copy-readtable nil)))
        (values (dispatch-macro-char-p #\$)
                (make-dispatch-macro-character #\$ nil)
                (dispatch-macro-char-p #\$))))
  nil t t)

(deftest ccl.bug#612-1
    (flet ((fn (x)
             (declare (optimize (safety 2) (speed 1)))
             (+ (load-time-value -14930786 t) 1826522792 x)
             ))
      (fn 0))
  1811592006)

(deftest ccl.bug#612-2
    (flet ((fn (x)
             (declare (optimize (safety 2)))
             (+ (load-time-value 1) 1826522792 x)))
      (fn 0))
  1826522793)

;;;  This test is bogus.  CCL::%INC-PTR's second argument
;;;  is documented to be a FIXNUM; if some 32-bit ports
;;;  complain that (EXPT 2 31) isn't a FIXNUM, they're
;;;  just being reasonable.
;;;  (This may have originally been intended to test the
;;;  x8664 compiler's ability to deal with 32-bit constants.)
#+bogus-test
(deftest ccl.bug#612-3
    (flet ((fn (p)
             (declare (optimize (safety 1) (speed 1)))
             (ccl::%inc-ptr p (expt 2 31))))
      (fn (ccl::%null-ptr))
      t)
  t)

(deftest ccl.symbol-macrolet-special
    (let ((x :special))
      (declare (special x))
      (symbol-macrolet ((x :symbol-macro))
        (values x (locally (declare (special x)) x))))
  :symbol-macro
  :special)

(deftest ccl.bug#617
    (flet ((test ()
             (declare (optimize (speed 1) (safety 1)))
             (symbol-macrolet ((inc 0.5))
               (loop with y = 0 do (incf y inc) while (< y 2)))))
      (test))
  nil)

(deftest ccl.bug#620
    (progn
      (test-compile (test-source-file "(defun ccl.bug#620.fn (buckets x y)
                                        (declare (type (simple-array t (* *)) buckets))
                                        (let ((result (aref buckets x y)))
                                            result))"))
      :win)
  :win)

(deftest ccl.bug#621
    (test-compiler-warning "(defun ccl.bug#621.fn ()
                              (the (values integer real) (round 2.5)))")
  ())

(deftest ccl.bug-defmethod-key-warning
         (progn
           (fmakunbound 'ccl.bug-defmethod-key-warning.gf)
           (defmethod ccl.bug-defmethod-key-warning.gf ((x integer) &key a))
           (test-compiler-warning "(in-package :cl-test)
                                   (defmethod ccl.bug-defmethod-key-warning.gf ((x string) &key) t)
                                   (defun ccl.bug-defmethod-key-warning.gf-caller (x a)
                                     (ccl.bug-defmethod-key-warning.gf x :a a))"))
  nil)

(deftest ccl.58983-1
    (test-compiler-warning "(defun ccl.58983-1 () (format t \"~A ~A\" 2 3 4))")
  (:format-error))

(deftest ccl.58983-2
    (test-compiler-warning "(defun ccl.58983-2 () (format t \"~a ~a ~2:*~a\" 1 2))")
  ())

(deftest ccl.58983-3
    (test-compiler-warning "(defun ccl.58983-3 () (format t \"~a ~a ~2:*\" 1 2))")
  (:format-error))

(deftest ccl.58983-4
    (test-compiler-warning "(defun ccl.58983-3 () (format t \"M~A ~A ~0@*~A\" 'adam \"I'M\"))")
  ())

(deftest ccl.defmethod-bad-lambda-list
    ;; This should warn, but not err out.
    (test-compiler-warning "(defmethod ccl.defmethod-bad-lambda-list ((s stream) s) s)")
  (:program-error))

(deftest ccl.bug#644
    (progn
      (test-compile (test-source-file "(defun test.bug#644 (x) (declare (optimize (speed 0) (safety 2) (debug 3)) (type (or null (function (t) t)) x)) x)
                                     (test.bug#644 (lambda (x) x))") :load t)
      :win)
  :win)

(deftest ccl.bug#645
    (let ((arr (make-array 5 :element-type 'single-float))
          (f (test-source-file  "~,,v,va" 30 #\null "")))
      (with-open-file (s f :direction :input :element-type '(unsigned-byte 8))
        (ccl:stream-read-ivector s arr 0 20)
        (aref arr 0)))
  0.0s0)

(deftest ccl.bug#660
    (progn
      (fmakunbound 'test.bug#660)
      (test-compile
       (test-source-file "(defun cl-test::test.bug#660 (x)
                           (declare (type (unsigned-byte ~d) x))
                           (ash x -1000))"
                         target::nbits-in-word)
       :load t)
      (test.bug#660 (ash 1 (1- target::nbits-in-word))))
  0)

(deftest ccl.bug#666
    (progn
      (fmakunbound 'test.bug#666)
      (test-compile
       (test-source-file "(defun cl-test::test.bug#666 (x y)
                            (declare (type fixnum x y))
                            (truncate x y))")
       :load t)
      (eql (test.bug#666 most-negative-fixnum -1) (abs most-negative-fixnum)))
  t)

(deftest ccl.bug#588
    (let ((*readtable* (copy-readtable)))
      (set-macro-character #\Left-Pointing_Double_Angle_Quotation_Mark
                           (lambda (stream ch)
                             (declare (ignore stream ch))
                             :win))
      (prog1
          (read-from-string (coerce '(#\Left-Pointing_Double_Angle_Quotation_Mark #\space) 'string))
        (set-macro-character #\Left-Pointing_Double_Angle_Quotation_Mark nil)))
  :win)

(deftest ccl.bug#708
    (flet ((one (b)
	     (declare (type (integer 51357426816569 68500595286128) b)
		      (optimize (speed 1) (safety 1)))
	     (logand b -2))
	   (two (b)
	     (logand b -2)))
      (- (one 67660763903986) (two 67660763903986)))
  0)

(deftest ccl.bug#735
  (flet ((diff (x)
	   (- (locally
		  (declare (type (integer 1000000000 2000000000) x))
		(lognor -10 x))
	      (lognor -10 x))))
    (diff 20))
  0)

(deftest ccl.bug#736
  (flet ((foo (a b)
	   (declare (type (integer -314476952 -84061465) a))
	   (declare (type (integer 16008 1204497162) b))
	   (logand b (the integer a))))
    (foo -299404531 1081111751))
  1075867653)

(deftest ccl.bug#828
  (float-sign (realpart (atan #c(-0d0 2d0))))
  -1d0)

(deftest ccl.bug#829
  (float-sign (imagpart (atanh #c(-2d0 -0d0))))
  -1d0)

(deftest ccl.bug#830
  (let ((val #c(1d300 1d300)))
    (handler-case
	(progn
	  (abs val)
	  :win)
      (floating-point-overflow (c) c)))
  :win)

(deftest ccl.bug#831
  (let ((val #c(1d300 1d300)))
    (handler-case
	(progn
	  (log val)
	  :win)
      (floating-point-overflow (c) c)))
  :win)

(deftest ccl.bug#832
  (let ((val #c(1d300 1d300)))
    (handler-case
	(progn
	  (sqrt val)
	  :win)
      (floating-point-overflow (c) c)))
  :win)

(deftest ccl.bug#674
  (let ((val #c(1d160 1)))
    (handler-case
	(progn
	  (/ val)
	  :win)
      (floating-point-overflow (c) c)))
  :win)

(deftest ccl.bug#840
  (progn
    (fmakunbound 'test.bug#840a)
    (fmakunbound 'test.bug#840b)
    (test-compile (test-source-file "(in-package :cl-test)
				     (declaim (inline test.bug#840a))
				     (defun test.bug#840a (x)
				       (+ x 42))
				     (defun test.bug#840b ()
				       (flet ((test.bug#840a (x y)
						(+ x y)))
					 (test.bug#840a 1 2)))")
		  :load t)
    :win)
  :win)

(deftest ccl.aset3
  (let ((m (make-array '(3 4 5) :initial-element 0)))
    (setf (aref m 2 3 4) 111)
    (eql 111 (aref m 2 3 4)))
  t)

(deftest ccl.format-goto-error
    (handler-case
	(format nil "This is an error ~*~a")
      (error (c)
	(handler-case (progn
			(ccl::report-condition c (make-broadcast-stream))
			:win)
	  (error (cc) :error))))
  :win)

;;; see http://clozure.com/pipermail/openmcl-devel/2011-July/012944.html
(deftest ccl.mul-strength-reduce-botch
  (flet ((foo ()
	   (let ((a 1))
	     (* 144115188075855873 a -1)))
	 (bar ()
	   (let ((a 1))
	     (* 33554433 a -1))))
    (values
     (= (foo) -144115188075855873)
     (= (bar) -33554433)))
  t t)

(deftest ccl.r15134
    (flet ((foo (a)
	     (declare (optimize safety)
		      (type (signed-byte 8) a))
	     a))
      (= (foo -41) -41))
  t)

(deftest ccl.arm-sbit-1
    (flet ((foo (a)
	     (sbit a 234)))
      (= 0 (foo #*1010010110010111101001001011000001010110101111001101001010110110001101000101010110000010101110011110100111001001011111000111100010010010101100111001001110111001001011001100010110001101101100011011001000001001101101001101111110101011000)))
  t)

(deftest ccl.arm-sbit-2
    (flet ((foo (a)
	     (sbit a 2)))
      (= 0 (foo #*1001)))
  t)

(deftest ccl.arm-char-constant
    (flet ((foo ()
	     #\LATIN_CAPITAL_LETTER_A_WITH_MACRON))
      (char= (foo) #\LATIN_CAPITAL_LETTER_A_WITH_MACRON))
  t)

(deftest ccl.%ilogxor2
    (let ((b (make-array 1 :element-type '(unsigned-byte 8)))
	  (m (make-array 1 :element-type 'fixnum :initial-element 3)))
      (setf (aref b 0) (logxor (aref m 0) (aref m 0)))
      (= (aref b 0) 0))
  t)

(deftest ccl.one-arg-float
    (flet ((foo (x)
	     (declare (type double-float x))
	     (float x)))
      (typep (foo 1d0) 'double-float))
  t)

