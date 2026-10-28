;;;; extended-tests/gc/heap-relocated-at-load.lisp -- an image that loads
;;;; with its heap away from the saved address.
;;;;
;;;; expected-state: clean
;;;;
;;;; When the OS does not give the heap its usual address (IMAGE_BASE), the
;;;; kernel moves the image by a bias at load time.  Two kernel globals were
;;;; not moved correctly on ARM64:
;;;;
;;;;   * WEAKVLL holds an untagged heap address, and the word-wise pass over
;;;;     the static area moves only tagged words.  It kept its old value, and
;;;;     the first ephemeral GC faulted in init_weakvll.
;;;;   * STATIC_CONSES, the head of the static-cons freelist, was moved twice,
;;;;     so it pointed outside the static-cons area.
;;;;
;;;; A full GC rebuilds both values without reading the old ones, so the
;;;; first GC after the load must be an ephemeral one, or the test sees
;;;; nothing.
;;;;
;;;; How it works.  A first child lisp makes a weak hash table, a weak
;;;; population and some static conses, and saves an image.  A second child
;;;; loads that image with CCL_FORCE_HEAP_RELOC=1, which tells the kernel to
;;;; reserve the heap with no address hint.  The second child checks the
;;;; static-cons freelist head, then runs an ephemeral GC, then checks the
;;;; weak objects and the static conses.  The parent bounds each child with
;;;; a timeout and kills a child that does not finish.
;;;;
;;;; A kernel that does not know CCL_FORCE_HEAP_RELOC loads the heap at its
;;;; saved address.  The second child then sees a bias of 0, and the case
;;;; is a SKIP, not a pass.
;;;;
;;;; Watched on linuxarm64 (kernel md5s in the commit message): a kernel with
;;;; the switch and no fixes faults in init_weakvll; with the WEAKVLL fix
;;;; only, the freelist head is out of the area; with both fixes it passes.

(in-package :cl-user)

(load (merge-pathnames "../harness.lisp" *load-pathname*))

(defparameter *hr-child-seconds* 300
  "How long the parent waits for one child before it kills the child.")

(defparameter *hr-save-forms* "
(in-package :cl-user)
(defvar *hr-keys* (loop for i below 100 collect (list i)))
(defvar *hr-weak* (make-hash-table :test 'eq :weak :key))
(dolist (k *hr-keys*) (setf (gethash k *hr-weak*) (car k)))
(defvar *hr-pop* (ccl:make-population :type :list :initial-contents *hr-keys*))
;; The first STATIC-CONS grows the area and uses one cell, so the saved
;; freelist is not empty.
(defvar *hr-static* (list (ccl::static-cons 'a 1)
                          (ccl::static-cons 'b 2)
                          (ccl::static-cons 'c 3)))
(defun hr-area-low ()
  (ccl::%fixnum-ref-natural (ccl::%get-kernel-global static-cons-area)
                            target::area.low))
(defun hr-area-high ()
  (ccl::%fixnum-ref-natural (ccl::%get-kernel-global static-cons-area)
                            target::area.high))
(defvar *hr-saved-area-low* (hr-area-low))
(defvar *hr-junk* nil)
(defun hr-say (fmt &rest args)
  (apply #'format t fmt args)
  (terpri)
  (finish-output))
(defun hr-egc-once ()
  (let ((n (ccl::%get-gc-count)))
    (loop repeat 100000
          while (= n (ccl::%get-gc-count))
          do (setq *hr-junk* (make-list 100)))
    (setq *hr-junk* nil)
    (/= n (ccl::%get-gc-count))))
(defun hr-weak-ok ()
  (and (= (hash-table-count *hr-weak*) 100)
       (every (lambda (k) (eql (gethash k *hr-weak*) (car k))) *hr-keys*)
       (= (length (ccl:population-contents *hr-pop*)) 100)
       (every (lambda (k) (member k (ccl:population-contents *hr-pop*) :test 'eq))
              *hr-keys*)))
(defun hr-static-ok ()
  (and (equal (mapcar #'car *hr-static*) '(a b c))
       (equal (mapcar #'cdr *hr-static*) '(1 2 3))))
(defun hr-check ()
  (handler-case
      (let* ((low (hr-area-low))
             (high (hr-area-high))
             (bias (- low *hr-saved-area-low*))
             (ok t))
        (hr-say \"HR-BIAS ~d\" bias)
        (when (zerop bias)
          (hr-say \"HR-RESULT SKIP the heap loaded at its saved address\")
          (ccl:quit 0))
        ;; Read the freelist head before anything allocates or collects.
        (let* ((head (ccl::%get-natural ccl::*static-cons-address* 0))
               (addr (logandc2 head target::fulltagmask)))
          (cond ((or (zerop head) (= head (ccl::%address-of nil)))
                 (hr-say \"HR-STATIC-HEAD EMPTY head=#x~x\" head))
                ((and (<= low addr) (< addr high))
                 (hr-say \"HR-STATIC-HEAD OK head=#x~x\" head))
                (t
                 (setq ok nil)
                 (hr-say \"HR-STATIC-HEAD BAD head=#x~x area=[#x~x,#x~x)\"
                         head low high))))
        (hr-say \"HR-EGC enabled=~s gcs=~d\" (ccl:egc-enabled-p)
                (ccl::%get-gc-count))
        (unless (ccl:egc-enabled-p)
          (hr-say \"HR-RESULT BAD the EGC is off, so no ephemeral GC can run\")
          (ccl:quit 1))
        (unless (hr-egc-once)
          (setq ok nil)
          (hr-say \"HR-EGC BAD no GC after 100000 allocations\"))
        (hr-say \"HR-WEAK-AFTER-EGC ~:[BAD~;OK~]\" (hr-weak-ok))
        (unless (hr-weak-ok) (setq ok nil))
        (hr-say \"HR-STATIC-AFTER-EGC ~:[BAD~;OK~]\" (hr-static-ok))
        (unless (hr-static-ok) (setq ok nil))
        (when ok
          (let ((new (list (ccl::static-cons 'd 4) (ccl::static-cons 'e 5))))
            (unless (and (equal (mapcar #'car new) '(d e))
                         (equal (mapcar #'cdr new) '(4 5))
                         (every (lambda (c)
                                  (let ((a (ccl::%address-of c)))
                                    (and (<= (hr-area-low) a)
                                         (< a (hr-area-high)))))
                                new))
              (setq ok nil)
              (hr-say \"HR-STATIC-NEW BAD\"))))
        (hr-egc-once)
        (gc)
        (unless (and (hr-weak-ok) (hr-static-ok))
          (setq ok nil)
          (hr-say \"HR-AFTER-FULL-GC BAD\"))
        (hr-say \"HR-RESULT ~:[BAD~;OK~]\" ok)
        (ccl:quit (if ok 0 1)))
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

(let* ((forms (hr-file "heap-reloc-tmp-save.lisp"))
       (image (hr-file "heap-reloc-tmp.image"))
       (stdin (hr-file "heap-reloc-tmp-stdin"))
       (out1 (hr-file "heap-reloc-tmp-save.out"))
       (out2 (hr-file "heap-reloc-tmp-load.out")))
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
             (xt-true (format nil "heap-reloc: the first child saves an image (rc ~s; ~{~a~^ | ~})"
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
                      (xt-skip "heap-reloc"
                               "the kernel loaded the heap at its saved address; it does not know CCL_FORCE_HEAP_RELOC"))
                     (t
                      (xt-true (format nil "heap-reloc: the image loads with the heap moved (rc ~s; ~a)"
                                       rc2 (or result
                                               (format nil "no result line; ~@[~a | ~]last output: ~{~a~^ | ~}"
                                                       (find-if (lambda (l) (search "xception" l)) lines)
                                                       (last lines 3))))
                               (and (eql rc2 0) result
                                    (string= result "HR-RESULT OK")))))))))
    (dolist (f (list forms image stdin out1 out2))
      (ignore-errors (delete-file f)))))

(xt-report "HEAP-RELOCATED-AT-LOAD")
