;;; weak-key-hash-gc.lisp -- weak-on-key hash tables must survive many
;;; threads that add and remove entries while the GC runs.
;;;
;;; Lisp makes a hash vector, then sets its weak flags.  A GC between the
;;; two can tenure the vector while it is not weak, and then no GC puts
;;; it on the weak list.  Later GCs still treat its keys and values as
;;; weak, so dead keys stay in the table and the values of live keys are
;;; not marked.  The table then holds pointers to freed memory.
;;;
;;; Eight threads add entries with fresh keys to two weak-on-key tables,
;;; one lock-free and one locked.  They remove some entries, and now and
;;; then they read every value back.  A correct lisp prints the PASS line
;;; and exits 0.  A broken one prints a bad value or an error, or dies in
;;; the GC.
;;;
;;; Contract:  WEAK-KEY-HASH-GC-RESULT :GCS n :VERDICT v

(in-package :cl-user)

(defparameter *seconds* 60
  "How long the threads run.")

(defvar *stop* nil)
(defvar *bad* nil)

(defun report (verdict)
  (format t "~&WEAK-KEY-HASH-GC-RESULT :GCS ~d :VERDICT ~a~@[ :WHAT ~a~]~%"
          (ccl:gccounts) verdict *bad*)
  (finish-output))

(defvar *tables*
  (list (make-hash-table :test 'eq :weak :key :shared t :lock-free :shared)
        (make-hash-table :test 'eq :weak :key :shared t :lock-free nil)))

(defstruct item tag (payload (make-array 5 :initial-element 0)))

(defun make-checked-item (tag)
  (let ((i (make-item :tag tag)))
    (setf (svref (item-payload i) 0) tag)
    i))

(defun item-ok-p (x)
  (and (item-p x)
       (simple-vector-p (item-payload x))
       (eql (svref (item-payload x) 0) (item-tag x))))

(defun fail (what)
  (unless *bad* (setq *bad* what))
  (setq *stop* t))

(defun check-table (h)
  (maphash (lambda (k v)
             (declare (ignore k))
             (unless (item-ok-p v)
               (fail (format nil "bad value ~s"
                             (ignore-errors (prin1-to-string v))))))
           h))

(defun worker (seed)
  (handler-case
      (let ((rs (make-random-state t))
            (n 0))
        (loop until *stop* do
          (dolist (h *tables*)
            (let* ((tag (+ seed (* 8 (random 100000 rs))))
                   (k (make-item :tag tag)))
              ;; The key dies at once unless this entry is checked.
              (setf (gethash k h) (make-checked-item tag))
              (when (zerop (mod n 7))
                (remhash k h))
              (when (zerop (mod n 1000))
                (check-table h))
              (incf n)))))
    (error (c)
      (fail (format nil "error ~a" c)))))

(dotimes (i 1000)
  (dolist (h *tables*)
    (setf (gethash (make-item :tag i) h) (make-checked-item i))))
;; Tenure the hash vectors.
(ccl:gc)

(let ((procs (loop for i below 8
                   collect (let ((seed i))
                             (ccl:process-run-function
                              (format nil "weak-key-~d" seed)
                              (lambda () (worker seed)))))))
  (loop repeat *seconds* until *stop* do (sleep 1))
  (setq *stop* t)
  (mapc #'ccl:join-process procs)
  (report (if *bad* "FAIL" "PASS"))
  (ccl:quit (if *bad* 1 0)))
