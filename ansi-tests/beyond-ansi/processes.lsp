;-*- Mode:     Lisp -*-
;;;; Author:   Jon Godbout
;;;; Created:  Sat May 10 06:37:41 2020
;;;; Contains: Tests for semaphore

(in-package :ba-test)

(compile-and-load "ba-aux.lsp")


;; Test semaphore

(deftest semaphore-init-to-0
    (let (test-object
	  (my-semaphore (make-semaphore)))
      (ccl:process-run-function
       "sem-caller"
       #'(lambda ()
	   (setf test-object 'a)
	   (wait-on-semaphore my-semaphore)
	   (setf test-object 'b)))
      (sleep .01)
      (assert (eq test-object 'a))
      (signal-semaphore my-semaphore)
      (sleep .01)
      (assert (eq test-object 'b)))
  t)


(deftest semaphore-init-to-0-two-processes
    (let (test-object
	  (my-semaphore (make-semaphore)))
      (ccl:process-run-function
       "sem-caller"
       #'(lambda ()
	   (push 'a test-object)
	   (wait-on-semaphore my-semaphore)
	   (push 'a test-object)))
      (ccl:process-run-function
       "sem-caller"
       #'(lambda ()
	   (push 'c test-object)
	   (wait-on-semaphore my-semaphore)
	   (push 'd test-object)))

      (sleep .01)
      (assert (or (eq test-object '(a c))
		  (eq test-object '(c a))))

      (signal-semaphore my-semaphore)
      (assert (and (or (member 'b test-object)
		       (member 'd test-object))
		   (or (not (member 'b test-object))
		       (not (member 'd test-object)))))

      (signal-semaphore my-semaphore)
      (sleep .01)
      (assert (and (member 'b test-object)
		   (member 'd test-object))))
  t)


(deftest semaphore-init-to-2
    (let (test-object
	  (my-semaphore (make-semaphore :count 2)))
      (ccl:process-run-function
       "sem-caller"
       #'(lambda ()
	   (setf test-object 'a)
	   (wait-on-semaphore my-semaphore)
	   (setf test-object 'b)))
      ;; Seamphore is immediately grabbed since count is positive.
      (sleep .01)
      (assert (eq test-object 'b))

      (ccl:process-run-function
       "sem-caller"
       #'(lambda ()
	   (setf test-object 'c)
	   (wait-on-semaphore my-semaphore)
	   (setf test-object 'd)))

      (sleep .01)
      (assert (eq test-object 'd))

      ;; Count becomes negative
      (ccl:process-run-function
       "sem-caller"
       #'(lambda ()
	   (setf test-object 'e)
	   (wait-on-semaphore my-semaphore)
	   (setf test-object 'f)))

      (sleep .01)
      (assert (eq test-object 'e))
      (signal-semaphore my-semaphore)
      (sleep .01)
      (assert (eq test-object 'f)))
  t)


(deftest semaphore-init-to-neg-1
    (let (test-object
	  (my-semaphore (make-semaphore :count -1)))
      (ccl:process-run-function
       "sem-caller"
       #'(lambda ()
	   (setf test-object 'a)
	   (wait-on-semaphore my-semaphore)
	   (setf test-object 'b)))

      (sleep .01)
      (assert (eq test-object 'a))
      (signal-semaphore my-semaphore)
      (sleep .01)
      (assert (eq test-object 'a)
	      (signal-semaphore my-semaphore)
	      (sleep .01)
	      (assert (eq test-object 'b)))
      t)
