;;; lock-free-clrhash-temp-vector.lisp -- CLRHASH of a big lock-free hash
;;; table installs a temporary vector that a concurrent GETHASH cannot probe.
;;;
;;; MECHANISM.  When a lock-free table is made for more than 1000 pairs, CLRHASH
;;; first installs a 1-pair vector, so the old vector can be collected
;;; before the new one is allocated.  Readers take no lock, so a GETHASH in
;;; another thread can probe that 1-pair vector.  The size-reciprocal of a
;;; 1-pair vector is 2^61, which is not a fixnum on a 64-bit target.  The
;;; probe code multiplies by it anyway, gets a wrong index and reads outside
;;; the vector.
;;;
;;; One writer thread puts 1500 keys into a shared lock-free EQ table made
;;; with :SIZE 2000, and clears it, in a loop.  One reader thread looks up those keys in a loop.
;;; Each lookup must give NIL or the key's own value.  A wrong value, an
;;; error in the reader, or a crash is a FAIL.  The run is bounded:
;;; RUN-SECONDS of work, then the result.  run-all.sh also runs it under an
;;; external timeout.
;;;
;;; Prints one result line and quits:
;;;   LOCK-FREE-CLRHASH-RESULT: PASS ...   exit 0
;;;   LOCK-FREE-CLRHASH-RESULT: FAIL ...   exit 42
;;; A crash ends the lisp with some other status, which is also a FAIL.
;;;
;;; Fixed by 94736219: clean 10 of 10 on linuxx8664 and linuxarm64 at
;;; 7aa04dc9.
;;; expected-state: clean

(in-package :cl-user)

(defparameter *keys* 1500)
(defparameter *run-seconds* 20)

(defparameter *size* 2000)

;;; CLRHASH sizes the new vector from the size the table was made with, so
;;; the 1-pair vector appears only for a table made with a :SIZE over 1000.
(defvar *table* (make-hash-table :test 'eq :shared t :lock-free t :size *size*))
(defvar *stop* nil)
(defvar *clears* 0)
(defvar *lookups* 0)
(defvar *found* 0)
(defvar *bad* nil)

(defun report (ok fmt &rest args)
  (format t "~&LOCK-FREE-CLRHASH-RESULT: ~:[FAIL~;PASS~] ~?~%" ok fmt args)
  (finish-output)
  (ccl:quit (if ok 0 42)))

(defun writer ()
  (handler-case
      (loop until *stop*
            do (dotimes (k *keys*)
                 (setf (gethash k *table*) k))
               (clrhash *table*)
               (incf *clears*))
    (serious-condition (c)
      (setq *bad* (list :writer (princ-to-string c)))
      (setq *stop* t))))

(defun reader ()
  (handler-case
      (loop until *stop*
            do (dotimes (k *keys*)
                 (multiple-value-bind (v found) (gethash k *table*)
                   (incf *lookups*)
                   (when found
                     (incf *found*)
                     (unless (eql v k)
                       (setq *bad* (list :key k :value v))
                       (setq *stop* t))))))
    ;; A fault in the lookup is a STORAGE-CONDITION, not an ERROR.
    (serious-condition (c)
      (setq *bad* (list :condition (princ-to-string c)))
      (setq *stop* t))))

(defun main ()
  (unless (ccl::hash-lock-free-p *table*)
    (report nil "setup: the table is not lock-free"))
  (format t "~&~a~%" (lisp-implementation-version))
  (finish-output)
  (let ((w (ccl:process-run-function "clrhash writer" #'writer))
        (r (ccl:process-run-function "gethash reader" #'reader))
        (end (+ (get-universal-time) *run-seconds*)))
    (loop until (or *stop* (>= (get-universal-time) end))
          do (sleep 0.1))
    (setq *stop* t)
    (dolist (p (list w r))
      (unless (ccl:join-process p :default nil)
        nil))
    (cond (*bad*
           (report nil "~s after ~d clears, ~d lookups" *bad* *clears* *lookups*))
          ((zerop *clears*)
           (report nil "setup: the writer never cleared the table"))
          (t
           (report t "~d clears, ~d lookups, ~d found" *clears* *lookups* *found*)))))

(main)
