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

;;; SBIT and (SETF SBIT) at constant indices (#755).  A backend accesses
;;; bits at small constant indices with an immediate offset, and at
;;; larger ones by computing the offset of the containing word at
;;; compile time; try indices on both sides of where each backend
;;; switches, with the index loaded into a register (safety 1) and not
;;; (safety 0), and compare against a variable index.
(defparameter *sbit-test-indices*
  (append (loop for i from 0 below 34 collect i)
          (loop for i from 32760 below 32800 collect i)
          (loop for i in '(130975 130976 130977 131103 131104 131105
                           262143 262144 1048479 1048480 1048481)
                collect i)))

(defun sbit-test-vector ()
  (let ((v (make-array 1048576 :element-type 'bit)))
    (dotimes (i (length v) v)
      (setf (sbit v i) (logand 1 (logcount (* 37 (1+ i))))))))

(deftest ccl.sbit.constant-index
    (let ((v (sbit-test-vector)))
      (loop for k in *sbit-test-indices*
            nconc
            (loop for safety in '(0 1)
                  for reader = (compile nil `(lambda (v)
                                               (declare (simple-bit-vector v)
                                                        (optimize (speed 3) (safety ,safety)))
                                               (sbit v ,k)))
                  nconc
                  (loop for val in '(0 1)
                        do (setf (sbit v k) val)
                        unless (eql (funcall reader v) val)
                          collect (list :read k safety val)))))
  nil)

(deftest ccl.setf-sbit.constant-index
    (let* ((v (sbit-test-vector))
           (expected (copy-seq v)))
      (loop for k in *sbit-test-indices*
            nconc
            (loop for safety in '(0 1)
                  nconc
                  (loop for val in '(0 1)
                        for const = (compile nil `(lambda (v)
                                                    (declare (simple-bit-vector v)
                                                             (optimize (speed 3) (safety ,safety)))
                                                    (setf (sbit v ,k) ,val)))
                        for var = (compile nil `(lambda (v x)
                                                  (declare (simple-bit-vector v) (bit x)
                                                           (optimize (speed 3) (safety ,safety)))
                                                  (setf (sbit v ,k) x)))
                        nconc
                        (loop for (kind . setter)
                                in (list (cons :const (lambda () (funcall const v)))
                                         (cons :var (lambda () (funcall var v val))))
                              do (setf (sbit v k) (- 1 val)
                                       (sbit expected k) val)
                              unless (and (eql (funcall setter) val)
                                          (equal v expected))
                                collect (list kind k safety val)
                              do (replace v expected))))))
  nil)
