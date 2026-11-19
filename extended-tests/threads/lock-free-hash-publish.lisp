;;; lock-free-hash-publish.lisp -- a lock-free hash table installs a new
;;; vector before the stores that fill its header are visible.
;;;
;;; MECHANISM.  CLRHASH of a lock-free table made for more than 1000 pairs
;;; conses a fresh vector, stores its header (size, size-reciprocal, the
;;; back pointer to the table, ...) and then stores the vector into the
;;; table with a plain store.  Readers take no lock.  On a target that
;;; keeps one thread's stores in order (x86-64) a reader that sees the new
;;; vector also sees its header.  On ARMv8 nothing orders the header
;;; stores before the install, so a reader on another core can see the
;;; new vector with a stale header.
;;;
;;; One writer puts 1001 keys into a lock-free EQ table made with :SIZE
;;; 2000 and clears it, in a loop.  Readers on the other cores watch the
;;; table's vector, and check the header of each new vector they see: the
;;; back pointer is the table, the size matches the vector length, and the
;;; size-reciprocal matches the size.  Any other value is a FAIL.  The run
;;; is bounded: RUN-SECONDS of work, then the result.  run-all.sh also
;;; runs it under an external timeout.
;;;
;;; Prints one result line and quits:
;;;   LOCK-FREE-HASH-PUBLISH-RESULT: PASS ...   exit 0
;;;   LOCK-FREE-HASH-PUBLISH-RESULT: FAIL ...   exit 42
;;; A crash ends the lisp with some other status, which is also a FAIL.
;;;
;;; Only the ARM ports reorder stores.  Measured on 4-core Graviton2:
;;; linuxarm64 and linuxarm reproduce 10 of 10 at 7aa04dc9, linuxx8664 is
;;; clean 10 of 10.  darwinarm64 has not been run.
;;; expected-state: clean
;;; expected-state linuxarm64: repro
;;; expected-state linuxarm: repro

(in-package :cl-user)

;;; The nhash.vector accessors are in xdump/hashenv.lisp, which the image
;;; does not keep.
(require "HASHENV" "ccl:xdump;hashenv")

(defparameter *keys* 1001)
(defparameter *run-seconds* 20)
(defparameter *size* 2000)

(defvar *table* (make-hash-table :test 'eq :shared t :lock-free t :size *size*))
(defvar *stop* nil)
(defvar *clears* 0)

(defun check-header (v h)
  "Return NIL when the header of V is the one CLRHASH stored, else a list
   of what was read."
  (let* ((n (ash (- (uvsize v) ccl::$nhash.vector_overhead) -1))
         (size (ccl::nhash.vector.size v))
         (recip (ccl::nhash.vector.size-reciprocal v))
         (back (ccl::nhash.vector.hash v)))
    (unless (and (eq back h)
                 (eql size n)
                 (plusp n)
                 (eql recip (floor (ash 1 (- target::nbits-in-word
                                             target::fixnumshift))
                                   n)))
      (list :pairs n :size size :reciprocal recip
            :back-pointer (if (eq back h) :table back)))))

(defun writer ()
  (loop until *stop*
        do (dotimes (i *keys*)
             (setf (gethash (1+ i) *table*) i))
           (clrhash *table*)
           (incf *clears*)))

(defun reader (result)
  (let ((seen nil) (checks 0) (bad 0) (sample nil))
    (loop until *stop*
          do (let ((v (ccl::nhash.vector *table*)))
               (unless (eq v seen)
                 (setq seen v)
                 (incf checks)
                 (let ((b (check-header v *table*)))
                   (when b
                     (incf bad)
                     (unless sample (setq sample b)))))))
    (setf (car result) (list checks bad sample))))

(let* ((nreaders (max 1 (min 3 (1- (ccl::cpu-count)))))
       (results (loop repeat nreaders collect (list nil)))
       (threads (cons (process-run-function "writer" #'writer)
                      (mapcar (lambda (r) (process-run-function "reader" #'reader r))
                              results))))
  (sleep *run-seconds*)
  (setq *stop* t)
  (dolist (p threads)
    (unless (process-wait-with-timeout "join" (* 10 ccl::*ticks-per-second*)
                                       (lambda () (process-exhausted-p p)))
      (format t "~&LOCK-FREE-HASH-PUBLISH-RESULT: FAIL a thread did not stop~%")
      (finish-output)
      (quit 42)))
  (let ((checks (reduce #'+ results :key #'caar))
        (bad (reduce #'+ results :key #'cadar))
        (sample (some #'caddar results)))
    (format t "~&LOCK-FREE-HASH-PUBLISH-RESULT: ~a ~a readers, ~a clears, ~a new vectors checked, ~a stale~@[, first ~s~]~%"
            (if (and (zerop bad) (plusp checks)) "PASS" "FAIL")
            nreaders *clears* checks bad sample)
    (finish-output)
    (quit (if (and (zerop bad) (plusp checks)) 0 42))))
