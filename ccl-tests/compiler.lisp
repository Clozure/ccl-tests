;;;-*-Mode: LISP; Package: CL-TEST -*-
;;;
;;; CCL-specific compiler tests: open-coded operations, type checks, and
;;; constant operands at the edges of what an instruction can encode.
;;; When an operation has both constant and variable forms, compile both
;;; and check them against each other and against the expected value.
;;;
;;; Like tests.lisp, this is loaded after ansi-tests/ccl.lsp, and the
;;; tests run with the current directory set to ansi-tests/.

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

;;; Closures over many values.  The captured values are stored at
;;; constant offsets in the new closure, and the higher ones may be
;;; beyond what a short immediate offset can reach.
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

;;; %get-bit and %set-bit take a signed bit offset from the pointer, and
;;; bit 0 is the least significant bit of the byte at the pointer.  The
;;; compiler open-codes constant and variable offsets and values
;;; differently, so try each combination, at small offsets of both signs
;;; and at byte offsets on either side of +/-4095, the largest immediate
;;; offset some backends can encode.
(defparameter *bit-test-offsets*
  (append (loop for i from -16 below 16 collect i)
          (loop for byte in '(4095 4096 5000 -4095 -4096 -5001)
                collect (* 8 byte)
                collect (+ 7 (* 8 byte)))))

;;; The tests use a buffer of 16K bytes, and offsets relative to the
;;; middle of it, initialized to this pattern.
(defun bit-test-byte (i)
  (logand #xff (* 37 (1+ i))))

(defun call-with-bit-test-buffer (fn)
  (ccl::%stack-block ((p 16384))
    (dotimes (i 16384)
      (setf (ccl::%get-unsigned-byte p i) (bit-test-byte i)))
    (funcall fn p (ccl::%inc-ptr p 8192))))

;;; Set the bit at OFF to the opposite of VAL, call SETTER, and check
;;; that it returned VAL, set the bit to VAL, and changed nothing else
;;; in the bytes around it.
(defun bit-test-set-ok (p off val setter)
  (let* ((target (+ 8192 (ash off -3)))
         (bit (logand off 7)))
    (loop for i from (- target 8) to (+ target 8)
          do (setf (ccl::%get-unsigned-byte p i) (bit-test-byte i)))
    (setf (ccl::%get-unsigned-byte p target)
          (dpb (- 1 val) (byte 1 bit) (bit-test-byte target)))
    (and (eql (funcall setter) val)
         (loop for i from (- target 8) to (+ target 8)
               always (eql (ccl::%get-unsigned-byte p i)
                           (if (eql i target)
                             (dpb val (byte 1 bit) (bit-test-byte i))
                             (bit-test-byte i)))))))

(deftest ccl.%get-bit
    (let ((variable (compile nil '(lambda (p off) (ccl::%get-bit p off)))))
      (call-with-bit-test-buffer
       (lambda (p q)
         (declare (ignore p))
         (loop for off in *bit-test-offsets*
               for expected = (ldb (byte 1 (logand off 7))
                                   (bit-test-byte (+ 8192 (ash off -3))))
               for constant = (compile nil `(lambda (p) (ccl::%get-bit p ,off)))
               unless (and (eql (funcall constant q) expected)
                           (eql (funcall variable q off) expected))
                 collect off))))
  nil)

(deftest ccl.%set-bit
    (let ((var-var (compile nil '(lambda (p off v)
                                  (setf (ccl::%get-bit p off) v))))
          (var-const (list (compile nil '(lambda (p off)
                                          (setf (ccl::%get-bit p off) 0)))
                           (compile nil '(lambda (p off)
                                          (setf (ccl::%get-bit p off) 1))))))
      (call-with-bit-test-buffer
       (lambda (p q)
         (loop for off in *bit-test-offsets*
               for const-var = (compile nil `(lambda (p v)
                                               (setf (ccl::%get-bit p ,off) v)))
               nconc
               (loop for val in '(0 1)
                     for const-const = (compile nil `(lambda (p)
                                                       (setf (ccl::%get-bit p ,off) ,val)))
                     nconc
                     (loop for (kind . setter)
                             in (list (cons :const-const (lambda () (funcall const-const q)))
                                      (cons :const-var (lambda () (funcall const-var q val)))
                                      (cons :var-const (lambda () (funcall (nth val var-const) q off)))
                                      (cons :var-var (lambda () (funcall var-var q off val))))
                           unless (bit-test-set-ok p off val setter)
                             collect (list kind off val)))))))
  nil)

;;; A NaN is unordered with every number, so = is false and /= is true when
;;; either argument is one, and ZEROP, MINUSP and PLUSP of a NaN are false.
;;; Each is called generic and open-coded, with the NaN on either side.  The
;;; invalid operation trap is masked, since CCL signals it for a NaN.  (The
;;; compares of a NaN with an integer or a ratio are in
;;; ccl.nan-rational-compare, and those with <, <=, > and >= in
;;; ccl.nan-compare.)
(defvar *nan-equality-zero* 0)

;;; T or NIL, or :ERROR if the call signals an error.
(defun nan-equality-value (fn &rest args)
  (handler-case (and (apply fn args) t)
    (error () :error)))

(defun nan-equality-failures ()
  (let* ((mode (ccl:get-fpu-mode)))
    (unwind-protect
         (let* ((failures nil))
           (ccl:set-fpu-mode :invalid nil)
           (dolist (type '(double-float single-float) failures)
             (let* ((one (if (eq type 'double-float) 1d0 1f0))
                    (zero (float *nan-equality-zero* one))
                    (nan (/ zero zero))
                    (other (list nan (- nan) one (- one) zero
                                 (if (eq type 'double-float) 1f0 1d0))))
               ;; (- nan) is a NaN with the opposite sign bit.
               (dolist (n (list nan (- nan)))
                 (dolist (y other)
                   (dolist (op '(= /=))
                     (let ((want (eq op '/=)))
                       (unless (and (eq want (nan-equality-value op n y))
                                    (eq want (nan-equality-value op y n)))
                         (push (list type op :generic n y) failures))))))
               (dolist (op '(= /=))
                 (let* ((fn (compile nil
                                     `(lambda (x y)
                                        (declare (type ,type x y)
                                                 (optimize (speed 3) (safety 0)))
                                        (,op x y))))
                        (self (compile nil
                                       `(lambda (x)
                                          (declare (type ,type x)
                                                   (optimize (speed 3) (safety 0)))
                                          (,op x x))))
                        (want (eq op '/=)))
                   (dolist (args (list (list nan one) (list one nan) (list nan nan)))
                     (unless (eq want (apply #'nan-equality-value fn args))
                       (push (list type op :open-coded) failures)))
                   (unless (eq want (nan-equality-value self nan))
                     (push (list type op :same-argument) failures))))
               (dolist (n (list nan (- nan)))
                 (when (or (nan-equality-value #'zerop n) (nan-equality-value #'minusp n)
                           (nan-equality-value #'plusp n))
                   (push (list type :predicate) failures))))))
      (apply #'ccl:set-fpu-mode mode))))

(deftest ccl.nan-equality
    (nan-equality-failures)
  nil)

;;; Ordinary compares of a float with zero and with a number of another
;;; type are the same as before.
(deftest ccl.float-equality
    (loop for (x y) in `((0d0 0) (-0d0 0) (0f0 0) (1d0 1) (1f0 1) (0.5d0 1/2)
                         (1d20 ,(expt 10 20)) (-1d0 -1))
          unless (and (= x y) (= y x) (not (/= x y)) (not (/= y x)))
            collect (list x y))
  nil)

(deftest ccl.float-inequality
    (loop for (x y) in `((1d0 0) (0d0 1) (1f0 2) (0.5d0 1/3) (1d0 ,(expt 10 30))
                         (1d-300 0) (-1d0 1) (1f0 1/3))
          unless (and (/= x y) (/= y x) (not (= x y)) (not (= y x)))
            collect (list x y))
  nil)

(deftest ccl.float-zerop-minusp
    (values (zerop 0d0) (zerop -0d0) (zerop 0f0) (zerop 1d-300) (zerop -1f0)
          (minusp -1d0) (minusp -0d0) (minusp 1d0) (minusp -1f-30) (minusp -0f0)
          (plusp 1d0) (plusp -0d0))
  t t t nil nil t nil nil t nil t nil)
