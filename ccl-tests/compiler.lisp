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
;;; compiler open-codes constant and variable offsets differently, so
;;; check negative offsets both ways.
(deftest ccl.%get-bit.negative-offset
    (let ((byte #b10100101)
          (variable (compile nil '(lambda (p off) (ccl::%get-bit p off)))))
      (ccl::%stack-block ((p 4))
        (dotimes (i 4) (setf (ccl::%get-unsigned-byte p i) 0))
        (setf (ccl::%get-unsigned-byte p 1) byte)
        (let ((q (ccl::%inc-ptr p 2)))
          (loop for off from -8 below 0
                for expected = (ldb (byte 1 (+ 8 off)) byte)
                for constant = (compile nil `(lambda (p) (ccl::%get-bit p ,off)))
                unless (and (eql (funcall constant q) expected)
                            (eql (funcall variable q off) expected))
                  collect off))))
  nil)

(deftest ccl.%set-bit.negative-offset
    (let ((variable (compile nil '(lambda (p off v)
                                   (setf (ccl::%get-bit p off) v)))))
      (ccl::%stack-block ((p 4))
        (let ((q (ccl::%inc-ptr p 2)))
          (loop for off from -8 below 0
                for mask = (ash 1 (+ 8 off))
                nconc (loop for v in '(0 1)
                            for setters
                              = (list (compile nil `(lambda (p)
                                                      (setf (ccl::%get-bit p ,off) ,v)))
                                      (compile nil `(lambda (p v)
                                                      (setf (ccl::%get-bit p ,off) v)))
                                      (lambda (p v) (funcall variable p off v)))
                            nconc (loop for setter in setters
                                        for k from 0
                                        do (dotimes (i 4)
                                             (setf (ccl::%get-unsigned-byte p i)
                                                   (if (eql v 0) #xff 0)))
                                           (if (eql k 0)
                                             (funcall setter q)
                                             (funcall setter q v))
                                        unless (equal (loop for i below 4
                                                            collect (ccl::%get-unsigned-byte p i))
                                                      (if (eql v 0)
                                                        (list #xff (logandc2 #xff mask) #xff #xff)
                                                        (list 0 mask 0 0)))
                                          collect (list off v k)))))))
  nil)
