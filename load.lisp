(in-package :cl-user)

(setf (logical-pathname-translations "tests")
      `(("**;*.*" ,(merge-pathnames "**/*.*" *load-truename*))))

(defun load-tests (&key (ansi t) (ccl t))
  (handler-bind ((warning
		  (lambda (c)
		    (if (typep c 'ccl::shadowed-typecase-clause)
		      (muffle-warning c)))))
    (cwd "tests:ansi-tests;")
    ;; This loads the infrastructure
    (load "gclload1.lsp")
    (when ansi
      (load "gclload2.lsp"))
    (when ccl
      (load "ccl.lsp"))))

(defun run-tests (&key verbose (compile t) exit (ansi t) (ccl t))
  (load-tests :ansi ansi :ccl ccl)
  (ccl:cwd "tests:ansi-tests;")
  (ccl:run-program "make" '("clean"))
  (let ((do-tests (find-symbol "DO-TESTS" "RT"))
	(failed (find-symbol "*FAILED-TESTS*" "RT")))
    (time (funcall do-tests :compile compile :verbose verbose :catch-errors t))
    (let ((failed-tests (symbol-value failed)))
      (when exit
	(ccl:quit (if failed-tests 1 0)))
      failed-tests)))
