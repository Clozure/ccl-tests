;;;; extended-tests/gc/static-conses-full-relocated.lisp -- static conses
;;;; that are all in use survive a load with the heap moved.
;;;;
;;;; expected-state: clean
;;;;
;;;; When the kernel moves the heap at load time, it must also move the
;;;; heap pointers held in the cars and cdrs of static conses.  It moved
;;;; them only when the static-cons freelist was not empty.  If every
;;;; static cons is in use at save time, the save-time GC leaves the
;;;; freelist empty, and the conses kept their old heap pointers.
;;;;
;;;; How it works.  A first child lisp takes static conses until the
;;;; freelist is empty, puts a fresh heap list in the car of each, and saves
;;;; an image.  A second child loads that image with CCL_FORCE_HEAP_RELOC=1,
;;;; which tells the kernel to reserve the heap with no address hint, and
;;;; reads every car before anything can collect.  The parent bounds each
;;;; child with a timeout and kills a child that does not finish.
;;;;
;;;; A kernel that does not know CCL_FORCE_HEAP_RELOC loads the heap at its
;;;; saved address.  The second child then sees a bias of 0, and the case
;;;; is a SKIP, not a pass.

(in-package :cl-user)

(load (merge-pathnames "../harness.lisp" *load-pathname*))

(defparameter *hr-child-seconds* 300
  "How long the parent waits for one child before it kills the child.")

(defparameter *hr-save-forms* "
(in-package :cl-user)
(defun hr-say (fmt &rest args)
  (apply #'format t fmt args)
  (terpri)
  (finish-output))
(defun hr-head ()
  (ccl::%get-natural ccl::*static-cons-address* 0))
(defun hr-head-empty-p ()
  (let ((head (hr-head)))
    (or (zerop head) (= head (ccl::%address-of nil)))))
(defun hr-area-low ()
  (ccl::%fixnum-ref-natural (ccl::%get-kernel-global static-cons-area)
                            target::area.low))
;; Take static conses until the freelist is empty.  Each car is a fresh
;; heap list that holds the index, so a stale car cannot read as correct.
(defvar *hr-static*
  (let ((all '()))
    (loop for i from 0 below 4000000
          do (push (ccl::static-cons (list i (* i 3)) i) all)
          until (hr-head-empty-p))
    all))
(defvar *hr-saved-area-low* (hr-area-low))
(hr-say \"HR-SAVE static conses ~d, freelist ~:[NOT EMPTY~;EMPTY~]\"
        (length *hr-static*) (hr-head-empty-p))
(defun hr-cell-ok (c)
  (let ((i (cdr c)) (l (car c)))
    (and (consp l) (eql (car l) i) (consp (cdr l)) (eql (cadr l) (* i 3)))))
(defun hr-check ()
  (handler-case
      (let* ((bias (- (hr-area-low) *hr-saved-area-low*)))
        (hr-say \"HR-BIAS ~d\" bias)
        (when (zerop bias)
          (hr-say \"HR-RESULT SKIP the heap loaded at its saved address\")
          (ccl:quit 0))
        (hr-say \"HR-STATIC-HEAD ~:[NOT EMPTY~;EMPTY~]\" (hr-head-empty-p))
        (unless (hr-head-empty-p)
          (hr-say \"HR-RESULT BAD the saved freelist was not empty, so this tests nothing\")
          (ccl:quit 1))
        ;; Read every car before anything can collect: a GC would follow
        ;; the stale pointers too.
        (let ((bad (count-if-not #'hr-cell-ok *hr-static*)))
          (hr-say \"HR-STATIC-CARS ~d of ~d bad\" bad (length *hr-static*))
          (gc)
          (let ((bad2 (count-if-not #'hr-cell-ok *hr-static*)))
            (hr-say \"HR-RESULT ~:[BAD~;OK~] ~d bad before a GC, ~d after\"
                    (and (zerop bad) (zerop bad2)) bad bad2)
            (ccl:quit (if (and (zerop bad) (zerop bad2)) 0 1)))))
    (error (c)
      (ignore-errors (hr-say \"HR-RESULT BAD error ~a\" c))
      (ccl:quit 1))))
(ccl:save-application (ccl:getenv \"HR_IMAGE\")
                      :toplevel-function #'hr-check
                      :purify nil)
")

(defun hr-file (name)
  "A scratch file next to this test file, so the test needs no setup.
The name carries the process id, so runs in parallel do not share files."
  (merge-pathnames (format nil "~d-~a" (ccl::getpid) name) *load-truename*))

(defun hr-run-child (args env output stdin)
  "Run the kernel with ARGS, wait at most *HR-CHILD-SECONDS*, and kill the
child if it is still running then.  Return the exit code, or :TIMEOUT."
  (let ((p (ccl:run-program (ccl::kernel-path) args
                            :wait nil :input stdin
                            :output output :if-output-exists :supersede
                            :error :output :env env)))
    (loop repeat (* 5 *hr-child-seconds*)
          while (eq (ccl:external-process-status p) :running)
          do (sleep 0.2))
    (if (eq (ccl:external-process-status p) :running)
      (progn
        (ccl:signal-external-process p 9)
        (loop repeat 50
              while (eq (ccl:external-process-status p) :running)
              do (sleep 0.1))
        :timeout)
      (nth-value 1 (ccl:external-process-status p)))))

(defun hr-file-lines (file)
  (with-open-file (s file :if-does-not-exist nil)
    (and s (loop for line = (read-line s nil) while line collect line))))

(defun hr-result-line (lines)
  (find-if (lambda (l) (eql 0 (search "HR-RESULT " l))) lines))

(let* ((forms (hr-file "static-full-tmp-save.lisp"))
       (image (hr-file "static-full-tmp.image"))
       (stdin (hr-file "static-full-tmp-stdin"))
       (out1 (hr-file "static-full-tmp-save.out"))
       (out2 (hr-file "static-full-tmp-load.out")))
  (unwind-protect
       (progn
         (with-open-file (s forms :direction :output :if-exists :supersede)
           (write-string *hr-save-forms* s))
         ;; An empty file, not /dev/null: with /dev/null on stdin a kernel
         ;; fault can stop in the debugger and print nothing.
         (close (open stdin :direction :output :if-exists :supersede))
         (ignore-errors (delete-file image))
         (let ((rc1 (hr-run-child
                     (list "-I" ccl::*heap-image-name* "-n" "-b" "-Q"
                           "-l" (namestring (truename forms)))
                     `(("HR_IMAGE" . ,(namestring image)))
                     out1 stdin)))
           (if (not (and (eql rc1 0) (probe-file image)))
             (xt-true (format nil "static-conses-full: the first child saves an image (rc ~s; ~{~a~^ | ~})"
                              rc1 (last (hr-file-lines out1) 5))
                      nil)
             (let* ((rc2 (hr-run-child
                          (list "-I" (namestring image) "-b")
                          '(("CCL_FORCE_HEAP_RELOC" . "1"))
                          out2 stdin))
                    (lines (hr-file-lines out2))
                    (result (hr-result-line lines)))
               (dolist (l lines)
                 (when (eql 0 (search "HR-" l))
                   (format t "~&  ~a~%" l)))
               (cond ((and result (eql 0 (search "HR-RESULT SKIP" result)))
                      (xt-skip "static-conses-full"
                               "the kernel loaded the heap at its saved address; it does not know CCL_FORCE_HEAP_RELOC"))
                     (t
                      (xt-true (format nil "static-conses-full: the image loads with the heap moved (rc ~s; ~a)"
                                       rc2 (or result
                                               (format nil "no result line; ~@[~a | ~]last output: ~{~a~^ | ~}"
                                                       (find-if (lambda (l) (search "xception" l)) lines)
                                                       (last lines 3))))
                               (and (eql rc2 0) result
                                    (eql 0 (search "HR-RESULT OK" result))))))))))
    (dolist (f (list forms image stdin out1 out2))
      (ignore-errors (delete-file f)))))

(xt-report "STATIC-CONSES-FULL-RELOCATED")
