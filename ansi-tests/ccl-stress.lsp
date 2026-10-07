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

;;; recursive_lock_trylock used to raise the recursion count before it
;;; decided it had not acquired the lock.  On the already-owned path with
;;; was_free NULL it incremented the count, fell through to the
;;; store_conditional, and returned EBUSY.  The caller, told it did not
;;; acquire, releases once for its one acquisition, and the lock stays
;;; owned for the life of the image.
;;;
;;; Nothing calls the function.  It is reachable through its kernel-import
;;; slot, which is how l0-aprims.lisp reaches new-recursive-lock, so this
;;; calls it the same way and hands it the calling thread's TCR converted
;;; the way level-0 converts it.  The owner comparison is therefore real.
;;;
;;; The observable is ownership seen from outside, not a count field.  One
;;; thread grabs the lock, calls trylock, releases once for the grab and
;;; once more only if trylock reported that it acquired.  A second thread
;;; then tries the lock.  A leaked count leaves the lock owned and the
;;; second thread cannot take it.  The second thread runs under a timeout,
;;; and the leak is released before returning either way.
(defun recursive-lock-trylock-count-leak (&key (timeout 20))
  (let ((lock (ccl:make-lock))
        (done (ccl:make-semaphore))
        (taken :unset)
        (prober nil)
        (rc nil))
    (unwind-protect
         (progn
           (ccl:grab-lock lock)
           (ccl:with-macptrs ((self))
             (ccl::%setf-macptr-to-object self (ccl::%current-tcr))
             (setq rc (ccl:ff-call
                       (ccl::%kernel-import
                        target::kernel-import-recursive-lock-trylock)
                       :address (ccl::recursive-lock-ptr lock)
                       :address self
                       :address (ccl::%null-ptr)
                       :signed-fullword)))
           (ccl:release-lock lock)
           (when (eql rc 0)
             (ccl:release-lock lock))
           (setq prober
                 (ccl:process-run-function "trylock-count-leak prober"
                   (lambda ()
                     (unwind-protect
                          (when (setq taken (ccl::try-lock lock))
                            (ccl:release-lock lock))
                       (ccl:signal-semaphore done)))))
           (cond ((not (ccl:timed-wait-on-semaphore done timeout))
                  (ccl:process-kill prober)
                  (list :raw-rc rc :prober :timeout))
                 ((eq taken t) nil)
                 (t (list :raw-rc rc :second-thread-acquired taken))))
      ;; A leaked count leaves this thread owning the lock.  Give back
      ;; whatever is left rather than return holding it.
      (dotimes (i 3)
        (unless (ignore-errors (ccl:release-lock lock) t) (return))))))

(deftest ccl.recursive-lock-trylock-count-leak
    (recursive-lock-trylock-count-leak)
  nil)

;;; process-interrupt used to lose a newly created process's initial
;;; function.  A thread stayed in the :reset state until it ran its
;;; startup function, so an interrupt sent right after
;;; process-run-function took thread-interrupt's :reset branch, which
;;; re-presets the process with a function that runs the interrupt and
;;; then resets the process.  The thread, already activated, ran that
;;; instead of its initial function, and stayed reset.  On x86-64 and
;;; arm64 this happened to nearly every process.
;;;
;;; Each process's initial function signals STARTED and then waits for
;;; RELEASE, and each interrupt signals INTERRUPTED.  Counting each
;;; semaphore's signals takes at most TIMEOUT seconds, so the test takes
;;; at most twice that.  Returns a list of what went wrong.
(defun process-interrupt-at-start (&key (nprocs 100) (timeout 30))
  (let ((started (ccl:make-semaphore))
        (interrupted (ccl:make-semaphore))
        (release (ccl:make-semaphore))
        (procs ())
        (failures ()))
    (flet ((count-signals (sem)
             (let ((deadline (+ (get-internal-real-time)
                                (* timeout internal-time-units-per-second))))
               (loop repeat nprocs
                     for remaining = (/ (- deadline (get-internal-real-time))
                                        (float internal-time-units-per-second))
                     while (and (plusp remaining)
                                (ccl:timed-wait-on-semaphore sem remaining))
                     count t))))
      (unwind-protect
           (progn
             (dotimes (i nprocs)
               (let ((p (ccl:process-run-function
                         "interrupt-at-start"
                         (lambda ()
                           (ccl:signal-semaphore started)
                           (ccl:timed-wait-on-semaphore release timeout)))))
                 (push p procs)
                 (ccl:process-interrupt
                  p (lambda () (ccl:signal-semaphore interrupted)))))
             (let ((nstarted (count-signals started))
                   (ninterrupted (count-signals interrupted)))
               (unless (eql nstarted nprocs)
                 (push (list :initial-functions-run nstarted) failures))
               (unless (eql ninterrupted nprocs)
                 (push (list :interrupts-run ninterrupted) failures))))
        (dotimes (i nprocs)
          (ccl:signal-semaphore release))
        (dolist (p procs)
          (unless (ccl:process-exhausted-p p)
            (ccl:process-kill p)))))
    (nreverse failures)))

(deftest ccl.process-interrupt-at-start
    (process-interrupt-at-start)
  nil)

;;; process-kill and process-abort work by interrupting the process.  When
;;; they're sent right after process-run-function, the interrupt must
;;; still run where the process-reset handler and abort restarts that
;;; they rely on are in place; otherwise the process just carries on.
;;; Each process would sleep for TIMEOUT seconds, so it exits by itself
;;; even if the kill is lost.  Each of the two rounds waits at most half
;;; of TIMEOUT, so the test takes at most TIMEOUT seconds.  Returns a
;;; list of what went wrong.
(defun process-kill-at-start (&key (nprocs 50) (timeout 30))
  (let ((failures ()))
    (dolist (how '(:kill :abort) (nreverse failures))
      (let* ((procs (loop repeat nprocs
                          collect (let ((p (ccl:process-run-function
                                            "kill-at-start"
                                            (lambda () (sleep timeout)))))
                                    (ecase how
                                      (:kill (ccl:process-kill p))
                                      (:abort (ccl:process-abort p)))
                                    p)))
             (survivors
              (progn
                (ccl:process-wait-with-timeout
                 "kill-at-start" (* (floor timeout 2) ccl:*ticks-per-second*)
                 (lambda () (every #'ccl:process-exhausted-p procs)))
                (remove-if #'ccl:process-exhausted-p procs))))
        (when survivors
          (push (list how :survivors (length survivors)) failures)
          (mapc #'ccl:process-kill survivors))))))

(deftest ccl.process-kill-at-start
    (process-kill-at-start)
  nil)

;;; On arm64, misc_ref and misc_set, the subprims behind uvref and
;;; (setf uvref) on a vector whose type isn't known at compile time,
;;; used to form a pointer into the vector in an imm register and then
;;; load or store through it.  The GC doesn't update imm registers, so a
;;; thread suspended for a GC between those two instructions read from
;;; or stored into the vector's old copy, and the store was lost.  With
;;; 8 threads filling fresh vectors while consing, a few stores were
;;; lost on nearly every run on darwinarm64 and linuxarm64.
;;;
;;; V is untyped in FILL-AND-CHECK, so both accesses go through the
;;; subprims.  Each thread is waited for at most TIMEOUT seconds.
;;; Returns a list of what went wrong.
(defun generic-uvset-gc-race (&key (nthreads 8) (rounds 40000) (timeout 60))
  (labels ((value (seed i)
             (logand (+ seed (* i 2654435761)) #xffffffff))
           (fill-and-check (v n seed)
             (dotimes (i n)
               (setf (ccl::uvref v i) (value seed i)))
             (loop for i below n
                   count (not (eql (ccl::uvref v i) (value seed i))))))
    (let* ((lock (ccl:make-lock))
           (wrong 0)
           (done (ccl:make-semaphore))
           (workers
            (loop for k below nthreads
                  collect (let ((k k))
                            (ccl:process-run-function
                             "generic-uvset-gc-race worker"
                             (lambda ()
                               (unwind-protect
                                    (let ((junk nil))
                                      (dotimes (r rounds)
                                        (let* ((v (make-array
                                                   64 :element-type
                                                   '(unsigned-byte 32)))
                                               (bad (fill-and-check
                                                     v 64 (+ r (* k 7919)))))
                                          ;; Cons, so that GCs happen.
                                          (setq junk (make-list 50))
                                          (when (plusp bad)
                                            (ccl:with-lock-grabbed (lock)
                                              (incf wrong bad)))))
                                      junk)
                                 (ccl:signal-semaphore done))))))))
      (cond ((not (loop repeat nthreads
                        always (ccl:timed-wait-on-semaphore done timeout)))
             (mapc #'ccl:process-kill workers)
             (list :timeout))
            ((plusp wrong) (list :wrong wrong))
            (t nil)))))

(deftest ccl.generic-uvset-gc-race
    (generic-uvset-gc-race)
  nil)

;;; On arm64, a catch frame saves the callee-saved node registers
;;; (save0-save3) on the temp stack, and leaving the catch normally
;;; restores them from there.  mkcatch used to store them while the
;;; frame was still raw, which the GC doesn't scan.  If a GC from
;;; another thread happened in between, the frame kept the registers'
;;; old values, and leaving the catch restored them, so a variable
;;; kept in one of those registers pointed at where its object used to
;;; be.  With 8 threads, this happened several times per run on
;;; darwinarm64.
;;;
;;; OBJ is live across the CATCH in HOLD, so it's kept in a callee-saved
;;; register.  Each thread is waited for at most TIMEOUT seconds.
;;; Returns a list of what went wrong.
(defun catch-nvr-gc-race (&key (nthreads 8) (rounds 10000) (timeout 60))
  (labels ((hold (obj copy n)
             (let ((bad 0))
               (dotimes (i n bad)
                 ;; Cons, so that GCs happen.
                 (catch 'catch-nvr-gc-race (make-list 20))
                 (unless (eq obj (car copy))
                   (incf bad)
                   (setq obj (car copy)))))))
    (let* ((lock (ccl:make-lock))
           (wrong 0)
           (done (ccl:make-semaphore))
           (workers
            (loop repeat nthreads
                  collect (ccl:process-run-function
                           "catch-nvr-gc-race worker"
                           (lambda ()
                             (unwind-protect
                                  (dotimes (r rounds)
                                    (let* ((obj (make-array 2))
                                           (bad (hold obj (list obj) 100)))
                                      (when (plusp bad)
                                        (ccl:with-lock-grabbed (lock)
                                          (incf wrong bad)))))
                               (ccl:signal-semaphore done)))))))
      (cond ((not (loop repeat nthreads
                        always (ccl:timed-wait-on-semaphore done timeout)))
             (mapc #'ccl:process-kill workers)
             (list :timeout))
            ((plusp wrong) (list :wrong wrong))
            (t nil)))))

(deftest ccl.catch-nvr-gc-race
    (catch-nvr-gc-race)
  nil)
