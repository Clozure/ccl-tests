;;;-*-Mode: LISP; Package: CL-TEST -*-
;;;
;;; CCL-specific tests that are slow or use several threads.  These
;;; are loaded with (run-tests :stress t), which is the default.

(in-package :cl-test)

;;; Concurrent writers to a lock-free hash table.  lock-free-puthash
;;; used to treat any key it found in the slot that nhash.find-new
;;; returned as its own, so when another thread claimed that free slot
;;; for a different key first, it overwrote that key's value.  One key
;;; got the wrong value and the other was never added.  With 4 writers
;;; this lost entries on nearly every run on x86-64 and arm64.
;;;
;;; Returns a list of the rounds that went wrong; each round waits a
;;; bounded time for its writers, so a regression can't hang the suite.
(defun lock-free-puthash-race (&key (nthreads 4) (nkeys 50000) (rounds 3)
                                    (timeout 60))
  (let ((failures ()))
    (dotimes (round rounds (nreverse failures))
      (let* ((h (make-hash-table :test 'equal :shared :lock-free))
             (done (ccl:make-semaphore))
             (writers
              (loop for k below nthreads
                    collect (let ((k k))
                              (ccl:process-run-function
                               "lock-free-puthash writer"
                               (lambda ()
                                 (unwind-protect
                                      (dotimes (i nkeys)
                                        (setf (gethash (format nil "~d-~d" k i) h)
                                              (list k i)))
                                   (ccl:signal-semaphore done))))))))
        (unless (loop repeat nthreads
                      always (ccl:timed-wait-on-semaphore done timeout))
          (mapc #'ccl:process-kill writers)
          (return (nreverse (cons (list round :timeout) failures))))
        (let ((bad (loop for k below nthreads
                         sum (loop for i below nkeys
                                   count (not (equal (gethash (format nil "~d-~d" k i) h)
                                                     (list k i)))))))
          (unless (and (eql (hash-table-count h) (* nthreads nkeys))
                       (eql bad 0))
            (push (list round :count (hash-table-count h) :bad bad)
                  failures)))))))

(deftest ccl.lock-free-puthash-race
    (lock-free-puthash-race)
  nil)
