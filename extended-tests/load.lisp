;;;; extended-tests/load.lisp -- load and run the extended tests.
;;;;
;;;; Path-independent by construction: every file finds its siblings through
;;;; *LOAD-TRUENAME*, so this works from any checkout with no installer, no
;;;; environment variable and no configuration.  That is the same mechanism the
;;;; top-level load.lisp uses for its "tests:" logical host.
;;;;
;;;; Usage:
;;;;     (load "extended-tests/load.lisp")
;;;;     (run-extended-tests)
;;;; or from the shell:
;;;;     make test-extended
;;;;
;;;; ⛔ WHAT IS NOT LOADED HERE.  Tests under threads/ that reproduce a HANG are
;;;; deliberately excluded.  See threads/README.md: a deadlock in the
;;;; suspend/resume path stops every other thread in the image, so no in-image
;;;; watchdog can report it -- the watchdog is stopped too.  Those reproducers
;;;; need an EXTERNAL timeout and are run one per process.

(in-package :cl-user)

(defparameter *extended-tests-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(defun extended-test-files ()
  "Every test file, in a stable order, excluding the harness and hang cases."
  (sort (remove-if
         (lambda (p) (search "threads" (namestring p)))
         (mapcan (lambda (dir)
                   (directory (merge-pathnames
                               (concatenate 'string dir "/*.lisp")
                               *extended-tests-directory*)))
                 '("backtrace" "compiler" "gc" "ffi" "calling-convention")))
        #'string< :key #'namestring))

(defun run-extended-tests (&key (exit nil))
  (load (merge-pathnames "harness.lisp" *extended-tests-directory*))
  ;; One image, many files: each test file ends by reporting, and a report that
  ;; QUITS would mean only the first file ever runs.
  (set (find-symbol "*REPORT-QUITS*" "CL-USER") nil)
  (let ((failed 0) (files 0))
    (dolist (f (extended-test-files))
      (incf files)
      (handler-case
          (let ((n (load f)))
            (declare (ignore n))
            (incf failed (or (symbol-value (find-symbol "*XT-FAILED*" "CL-USER")) 0)))
        (serious-condition (c)
          (format t "~&XT-LOAD-ERROR ~a: ~a~%" (file-namestring f) c)
          (incf failed))))
    (format t "~&XT-SUITE FILES ~d FAILED ~d~%" files failed)
    (finish-output)
    (when exit (ccl:quit (if (zerop failed) 0 1)))
    failed))
