;;; page16k-commit.lisp -- the GC structures must be committed on a
;;; Linux kernel with 16 KiB pages.
;;;
;;; Some arm64 Linux kernels use 16 KiB pages (Raspberry Pi 5, Asahi).
;;; There mmap with MAP_FIXED and mprotect refuse an address that is not
;;; a multiple of 16 KiB.  The kernel commits parts of its mark bits,
;;; reference index and relocation table at addresses that are only
;;; 4 KiB aligned, and it ignores the error.  The lisp later enters the
;;; kernel debugger and aborts.
;;;
;;; The test does not need a 16 KiB kernel.  page16k-shim.c, preloaded
;;; into a child lisp, makes this kernel act as one: page size 16384,
;;; EINVAL for an address that is not 16 KiB aligned.  It runs three
;;; children under it:
;;;   :BOOT    start, GC, exit.  The shim must not break a plain start; if
;;;            it does, the verdict is SHIM-BROKEN, not FAIL.
;;;   :SMALL   start with -R 1g, so the GC structures sit at an address
;;;            that is 4 KiB but not 16 KiB aligned, then GC.
;;;   :STATIC  make 300000 static conses, which moves the heap start down
;;;            in 4 KiB steps, then GC and check every cons.
;;;
;;; Needs cc.  Linux arm64 only; other targets report a skip.
;;;
;;; expected-state: clean
;;; expected-state linuxarm64: repro
;;;
;;; Contract: the parent prints one line per child,
;;;   PAGE16K-COMMIT-CHILD <name> :STATUS s :CODE n :OK t|nil
;;; then its own verdict last:
;;;   PAGE16K-COMMIT-VERDICT PASS|FAIL <names>|SHIM-BROKEN|NO-SHIM|SKIP

(in-package :cl-user)

(defparameter *child-seconds* 120
  "The parent kills a child that runs longer than this.")

(defun verdict (v code)
  (format t "~&PAGE16K-COMMIT-VERDICT ~a~%" v)
  (finish-output)
  (quit code))

(unless (and (member :arm64-target *features*)
             (member :linux-target *features*))
  (verdict "SKIP" 0))

(defparameter *forms*
  '((:boot ()
     "(progn (gc) (format t \"~&PAGE16K-OK~%\") (quit 0))")
    (:small ("-R" "1g")
     "(progn (gc) (gc) (format t \"~&PAGE16K-OK~%\") (quit 0))")
    (:static ()
     "(let ((l nil))
        (dotimes (i 300000) (push (ccl::static-cons i nil) l))
        (gc) (gc)
        (let ((n 300000))
          (dolist (c l) (decf n) (unless (eql (car c) n) (quit 3))))
        (format t \"~&PAGE16K-OK~%\") (quit 0))")))

(defun build-shim ()
  (let* ((src (merge-pathnames "page16k-shim.c" *load-truename*))
         (dir (or (ccl::getenv "TMPDIR") "/tmp"))
         (so (format nil "~a/page16k-shim-~d.so" dir (ccl::getpid)))
         (p (ccl:run-program "cc" (list "-shared" "-fPIC" "-O2" "-o" so
                                        (native-translated-namestring src) "-ldl")
                             :output t :error :output)))
    (multiple-value-bind (status code) (ccl:external-process-status p)
      (unless (and (eq status :exited) (eql code 0) (probe-file so))
        (verdict (format nil "NO-SHIM cc ~a ~a" status code) 1)))
    so))

(defun run-child (so args form)
  "Run one child lisp under the shim.  Return (values ok status code lines)."
  (let* ((p (ccl:run-program (ccl::kernel-path)
                             (append args
                                     (list "-I" ccl::*heap-image-name* "-n" "-b" "-Q"
                                           "-e" form))
                             :wait nil :input :stream :output :stream :error :output
                             :env (list (cons "LD_PRELOAD" so))))
         (out (ccl:external-process-output-stream p)))
    (loop repeat (* 10 *child-seconds*)
          while (eq (ccl:external-process-status p) :running)
          do (sleep 0.1))
    (when (eq (ccl:external-process-status p) :running)
      (ccl:signal-external-process p 9)
      (loop repeat 50
            while (eq (ccl:external-process-status p) :running)
            do (sleep 0.1)))
    (let ((lines (loop for line = (read-line out nil) while line collect line)))
      (multiple-value-bind (status code) (ccl:external-process-status p)
        (values (and (eq status :exited) (eql code 0)
                     (some (lambda (l) (search "PAGE16K-OK" l)) lines))
                status code lines)))))

(let ((so (build-shim))
      (failed '()))
  (dolist (spec *forms*)
    (destructuring-bind (name args form) spec
      (multiple-value-bind (ok status code lines) (run-child so args form)
        (format t "~&PAGE16K-COMMIT-CHILD ~a :STATUS ~a :CODE ~a :OK ~a~%"
                name status code (and ok t))
        (unless (some (lambda (l) (search "page16k-shim: loaded" l)) lines)
          (verdict (format nil "NO-SHIM ~a did not load it" name) 1))
        ;; At most 8 refused calls and the last lines, so a crash shows.
        (let ((refused (remove-if-not (lambda (l) (search "EINVAL" l)) lines)))
          (format t "~&;;   ~d call(s) refused~%" (length refused))
          (dolist (l (subseq refused 0 (min 8 (length refused))))
            (format t "~&;;   ~a~%" l)))
        (unless ok
          (dolist (l (last lines 6))
            (format t "~&;;   | ~a~%" l))
          (when (eq name :boot)
            (verdict "SHIM-BROKEN" 2))
          (push name failed)))))
  (finish-output)
  (if failed
      (verdict (format nil "FAIL~{ ~a~}" (reverse failed)) 1)
      (verdict "PASS" 0)))
