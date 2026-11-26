;;; hard-stack-overflow-reset.lisp -- after a hard value-stack overflow,
;;; the lisp must throw to the toplevel and keep working.
;;;
;;; A storage-condition handler that recurses again, without unwinding,
;;; runs through the open soft guard page into the hard guard page.  The
;;; kernel then has to throw the thread to the toplevel.  If it does that
;;; from inside the signal handler, the handler never returns and the
;;; thread keeps the exception lock.  The next trap on another thread then
;;; waits for that lock for good.
;;;
;;; So the test has two stages:
;;;
;;;   1. The overflow throws to the toplevel.  An unwind-protect around
;;;      it sees the throw go past, and runs stage 2.
;;;   2. Another thread conses until it takes allocation traps, then GCs.
;;;      Both go through the exception handler.
;;;
;;; A correct lisp prints the PASS line and exits 0.  A broken one exits
;;; with an error, dies in the kernel, or waits forever, and only the
;;; external timeout sees the last case.
;;;
;;; DEEP takes many required arguments, so that each frame uses much more
;;; value stack than control stack, and the value stack runs out first.
;;;
;;; Contract:  HARD-STACK-OVERFLOW-RESULT :STAGE s :VERDICT v

(in-package :cl-user)

(defvar *stage* "START")

(defun report (verdict)
  (format t "~&HARD-STACK-OVERFLOW-RESULT :STAGE ~a :VERDICT ~a~%"
          *stage* verdict)
  (finish-output))

(defmacro define-deep (nargs)
  (let ((args (loop for i from 1 to nargs
                    collect (intern (format nil "A~d" i)))))
    `(progn
       (defun deep (n ,@args)
         (if (<= n 0)
             0
             ;; Not a tail call: the frame has to stay live.
             (+ 1 (deep (1- n) ,@args))))
       (defun call-deep (n)
         (apply #'deep n (make-list ,nargs :initial-element 0))))))

(define-deep 100)

(defun trap-on-another-thread ()
  (let* ((done (make-semaphore))
         (ok nil)
         (p (process-run-function
             "after-reset"
             (lambda ()
               (unwind-protect
                    (let ((junk nil))
                      (dotimes (i 20000)
                        (push (make-list 8 :initial-element i) junk))
                      (setq junk nil)
                      (gc)
                      (setq ok t))
                 (signal-semaphore done))))))
    (cond ((not (timed-wait-on-semaphore done 30))
           (ignore-errors (process-kill p))
           "TIMEOUT")
          (ok "PASS")
          (t "FAIL"))))

(defvar *returned* nil)

;;; Bound again in the handler, just after a catch.  The throw to the
;;; toplevel has to undo that binding, and the reset must not overwrite
;;; it on the way.
(defvar *bound* :global)

;;; A throw to the toplevel runs this cleanup on its way out, whichever
;;; catch receives it.  So stage 2 runs here, and this is the only place
;;; that exits 0.
(setq *stage* "OVERFLOW")
(unwind-protect
     (let ((soft 0))
       (handler-case
           (handler-bind
               ((storage-condition
                  (lambda (c)
                    (declare (ignore c))
                    (when (= (incf soft) 1)
                      ;; The binding sits right under the newest catch.
                      (catch 'inner
                        (let ((*bound* :inner))
                          (call-deep most-positive-fixnum)))))))
             (call-deep most-positive-fixnum))
         (error (c)
           (setq *returned* t)
           (report (format nil "NO-RESET-~a" (type-of c)))
           (quit 1)))
       (setq *returned* t))
  (unless *returned*
    (setq *stage* "RESET")
    (let ((verdict (if (eq *bound* :global)
                     (handler-case (trap-on-another-thread)
                       (error (c) (format nil "ERROR-~a" (type-of c))))
                     "BINDING-NOT-UNDONE")))
      (setq *stage* "AFTER-RESET")
      (report verdict)
      (quit (if (equal verdict "PASS") 0 1)))))

;;; Reached only when the overflow returned without a reset.
(report "NO-RESET")
(quit 1)
