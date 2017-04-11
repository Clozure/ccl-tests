(in-package :cl-user)

(setf (logical-pathname-translations "tests")
      `(("**;*.*" ,(merge-pathnames "**/*.*" *load-truename*))))

(defun load-tests ()
  (handler-bind ((warning
		  (lambda (c)
		    (if (typep c 'ccl::shadowed-typecase-clause)
		      (muffle-warning c)))))
    (cwd "tests:ansi-tests;")
    ;; This loads the infrastructure
    (load "gclload1.lsp")
    (load "gclload2.lsp")
    (load "ccl.lsp")))

(defun run-tests (&key verbose (compile t) exit)
  (load-tests)
  (ccl:cwd "tests:ansi-tests;")
  (ccl:run-program "make" '("clean"))
  (let ((do-tests (find-symbol "DO-TESTS" "RT"))
	(failed (find-symbol "*FAILED-TESTS*" "RT")))
    (time (funcall do-tests :compile compile :verbose verbose :catch-errors t))
    (let ((failed-tests (symbol-value failed)))
      (when exit
	(ccl:quit (if failed-tests 1 0)))
      failed-tests)))
