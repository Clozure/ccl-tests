;;;; windows-pipe-read.lisp -- reads from run-program pipes on Windows
;;;;
;;;; Run it in its own process, under an external timeout:
;;;;
;;;;     wx86cl64 -n -b -l windows-pipe-read.lisp
;;;;
;;;; It starts child processes with RUN-PROGRAM.  Each child is a second
;;;; copy of the same lisp (the same kernel and the same image), so the
;;;; test needs nothing that a CCL release does not have.
;;;;
;;;; Cases:
;;;;   roundtrip   send N short lines, one at a time, to a child that
;;;;               echoes them; wait for each reply, and check every reply.
;;;;   interrupt   a thread that is blocked reading a silent pipe runs a
;;;;               PROCESS-INTERRUPT function promptly, and then its read
;;;;               gets the next line.
;;;;   interrupt-race  the same, with the interrupt at random times near
;;;;               the start of the read, in 5 rounds of 100.  It runs first.
;;;;   gc          a thread blocked reading a pipe survives full GCs done
;;;;               by another thread, and then gets its line intact.
;;;;   listen      LISTEN on a stream from another thread returns while a
;;;;               reader is blocked on the same stream.
;;;;   eof         a reader gets the child's last line and then end of
;;;;               file, both when the child exits before the read and
;;;;               while the read is blocked.
;;;;   output      :OUTPUT :STREAM and :OUTPUT to a lisp stream both get
;;;;               all of the child's output.
;;;;
;;;; A blocked operation in a case happens in a separate thread, and the
;;;; test waits for it with a timeout, so a case that fails reports FAIL
;;;; and the test goes on.  A thread that is still stuck at the end
;;;; cannot keep the process alive: the test ends with ExitProcess.
;;;; The external timeout is still necessary, because a defect can stop
;;;; the whole lisp.
;;;;
;;;; The last line is
;;;;     XT-RESULT WINDOWS-PIPE-READ TOTAL <n> FAILED <n>
;;;; and the exit status is 0 only if FAILED is 0.

(in-package :cl-user)

#-windows-target
(progn
  (format t "~&XT-RESULT WINDOWS-PIPE-READ TOTAL 0 FAILED 0 (not Windows)~%")
  (ccl:quit 0))

(defvar *cases* '(case-interrupt-race case-roundtrip case-interrupt case-gc case-listen case-eof case-output)
  "The cases to run.  Bind it before loading this file to run fewer.")
(defvar *total* 0)
(defvar *failed* 0)

(defun note (verdict name fmt &rest args)
  (format t "~&CASE ~a ~a~@[ ~a~]~%" name verdict
          (and fmt (apply #'format nil fmt args)))
  (finish-output))

(defun check (name ok fmt &rest args)
  (incf *total*)
  (unless ok (incf *failed*))
  (apply #'note (if ok "PASS" "FAIL") name fmt args)
  ok)

(defun skip (name why)
  (note "SKIP" name "~a" why))

(defun now-ms ()
  (/ (get-internal-real-time) (/ internal-time-units-per-second 1000)))

;;; Children

(defun child (form)
  "Start a copy of this lisp that evaluates FORM, with its standard input
and output on pipes."
  (ccl:run-program (ccl::kernel-path)
                   (list "-I" ccl::*heap-image-name* "-n" "-b"
                         "-e" (let ((*package* (find-package :cl-user))
                                    (*print-readably* nil))
                                (prin1-to-string form)))
                   :input :stream :output :stream :error :output
                   :wait nil))

(defparameter *echo-form*
  '(loop (let ((l (read-line *standard-input* nil nil)))
           (if l
             (progn (write-line l) (finish-output))
             (ccl:quit 0)))))

(defun kill-child (proc)
  (ignore-errors (close (ccl:external-process-input-stream proc)))
  (let ((h (ccl:external-process-id proc)))
    (when (eq (ccl:external-process-status proc) :running)
      (ignore-errors
        (ccl:external-call "TerminateProcess"
                           :address (if (typep h 'ccl::macptr) h (ccl::%int-to-ptr h))
                           :unsigned-fullword 1
                           :signed-fullword)))))

;;; Threads with a result and a timeout

(defstruct job process done result)

(defun start-job (name fn)
  (let ((job (make-job :done (ccl:make-semaphore))))
    (setf (job-process job)
          (ccl:process-run-function
           name
           (lambda ()
             (setf (job-result job)
                   (handler-case (list :ok (funcall fn))
                     (error (c) (list :error (princ-to-string c)))))
             (ccl:signal-semaphore (job-done job)))))
    job))

(defun wait-job (job seconds)
  "Return (:ok value), (:error text), or :timeout."
  (if (ccl:timed-wait-on-semaphore (job-done job) seconds)
    (job-result job)
    :timeout))

(defun wait-until-blocked (job)
  ;; There is no portable way to see that a thread is inside the read.
  ;; Give it time to get there.  If it is not there yet, the case still
  ;; tests an interrupt that arrives just before the read.
  (declare (ignore job))
  (sleep 0.5))

;;; Cases

(defun case-roundtrip (&optional (n 200))
  (let* ((proc (child *echo-form*))
         (in (ccl:external-process-input-stream proc))
         (out (ccl:external-process-output-stream proc))
         (bad 0))
    (unwind-protect
         (let ((job (start-job
                     "roundtrip"
                     (lambda ()
                       ;; Warm up: the first reply also waits for the child
                       ;; to start.
                       (dotimes (i 5)
                         (write-line "warm" in) (finish-output in)
                         (read-line out nil nil))
                       (let ((t0 (now-ms)))
                         (dotimes (i n)
                           (let ((msg (format nil "request ~d" i)))
                             (write-line msg in) (finish-output in)
                             (unless (equal (read-line out nil nil) msg)
                               (incf bad))))
                         (/ (- (now-ms) t0) n))))))
           (let ((r (wait-job job 120)))
             (cond ((eq r :timeout)
                    (check "roundtrip" nil "timed out after 120 s"))
                   ((eq (first r) :error)
                    (check "roundtrip" nil "signalled ~a" (second r)))
                   (t
                    (check "roundtrip-replies" (zerop bad)
                           "~d of ~d replies wrong" bad n)))))
      (kill-child proc))))

(defun case-interrupt ()
  (let* ((proc (child *echo-form*))
         (in (ccl:external-process-input-stream proc))
         (out (ccl:external-process-output-stream proc))
         (ran (ccl:make-semaphore))
         (reader (start-job "blocked reader"
                            (lambda () (read-line out nil :eof)))))
    (unwind-protect
         (progn
           (wait-until-blocked reader)
           (let ((t0 (now-ms)) (latency nil))
             (ccl:process-interrupt (job-process reader)
                                    (lambda ()
                                      (setq latency (- (now-ms) t0))
                                      (ccl:signal-semaphore ran)))
             (check "interrupt-runs" (ccl:timed-wait-on-semaphore ran 5)
                    "latency ~@[~,1f ~]ms" (and latency (float latency))))
           (check "interrupt-read-not-done" (null (job-result reader))
                  "result before the line was sent: ~s" (job-result reader))
           (write-line "after interrupt" in) (finish-output in)
           (let ((r (wait-job reader 10)))
             (check "interrupt-read-resumes" (equal r '(:ok "after interrupt"))
                    "~s" r)))
      (kill-child proc))))

(defun race-round (n)
  ;; One thread reads N lines.  Before each line is sent, it is
  ;; interrupted at a random time, so some interrupts arrive while it
  ;; is inside the read and some just before it enters the read.
  ;; Return the number of interrupts that did not run, and whether the
  ;; reader got every line.
  (let* ((proc (child *echo-form*))
         (in (ccl:external-process-input-stream proc))
         (out (ccl:external-process-output-stream proc))
         (lost 0)
         (started (ccl:make-semaphore))
         (reader (start-job "racing reader"
                            (lambda ()
                              (ccl:signal-semaphore started)
                              (loop repeat n collect (read-line out nil :eof))))))
    (unwind-protect
         (progn
           ;; Interrupt the reader only after its function runs.  This
           ;; case is about the read, not about thread startup.
           (ccl:timed-wait-on-semaphore started 10)
           (dotimes (i n)
             (let ((ran (ccl:make-semaphore)))
               (sleep (random 0.003))
               (ccl:process-interrupt (job-process reader)
                                      (lambda () (ccl:signal-semaphore ran)))
               (unless (ccl:timed-wait-on-semaphore ran 5)
                 (incf lost))
               (write-line (format nil "line ~d" i) in)
               (finish-output in)))
           (values lost
                   (equal (wait-job reader 30)
                          (list :ok (loop for i below n
                                          collect (format nil "line ~d" i))))))
      (kill-child proc))))

(defun case-interrupt-race (&optional (rounds 5) (n 100))
  ;; Each round has a new child and a new reader.  The deadlock does not
  ;; occur on every run, so more rounds make one run more likely to show it.
  (let ((lost 0) (bad 0))
    (dotimes (r rounds)
      (multiple-value-bind (l ok) (race-round n)
        (incf lost l)
        (unless ok (incf bad))))
    (check "interrupt-race-all-run" (zerop lost)
           "~d of ~d interrupts did not run within 5 s" lost (* rounds n))
    (check "interrupt-race-reads" (zerop bad)
           "~d of ~d rounds did not get every line" bad rounds)))

(defun case-gc ()
  (let* ((proc (child *echo-form*))
         (in (ccl:external-process-input-stream proc))
         (out (ccl:external-process-output-stream proc))
         (reader (start-job "blocked reader"
                            (lambda () (read-line out nil :eof)))))
    (unwind-protect
         (progn
           (wait-until-blocked reader)
           (let ((gcs (start-job "gc"
                                 (lambda ()
                                   (dotimes (i 20)
                                     (make-list 10000)
                                     (ccl:gc))
                                   20))))
             (let ((r (wait-job gcs 60)))
               (check "gc-runs" (equal r '(:ok 20)) "~s" r)))
           (check "gc-read-not-done" (null (job-result reader))
                  "result before the line was sent: ~s" (job-result reader))
           (write-line "after gc" in) (finish-output in)
           (let ((r (wait-job reader 10)))
             (check "gc-read-resumes" (equal r '(:ok "after gc")) "~s" r)))
      (kill-child proc))))

(defun case-listen ()
  (let* ((proc (child *echo-form*))
         (in (ccl:external-process-input-stream proc))
         (out (ccl:external-process-output-stream proc))
         (reader (start-job "blocked reader"
                            (lambda () (read-line out nil :eof)))))
    (unwind-protect
         (progn
           (wait-until-blocked reader)
           ;; Streams are :private by default, so use a stream shared
           ;; between threads only through its fd.
           (let* ((fd (ccl::stream-device out :input))
                  (lj (start-job "listen"
                                 (lambda ()
                                   (ccl::fd-input-available-p fd 0)))))
             (let ((r (wait-job lj 5)))
               (check "listen-returns" (equal r '(:ok nil)) "~s" r)))
           (write-line "after listen" in) (finish-output in)
           (let ((r (wait-job reader 10)))
             (check "listen-read-resumes" (equal r '(:ok "after listen")) "~s" r)))
      (kill-child proc))))

(defun case-eof ()
  ;; The child writes a line and exits before the read.
  (let* ((proc (child '(progn (write-line "last") (finish-output) (ccl:quit 0))))
         (out (ccl:external-process-output-stream proc))
         (job (start-job "eof reader"
                         (lambda ()
                           (list (read-line out nil :eof)
                                 (read-line out nil :eof))))))
    (let ((r (wait-job job 30)))
      (check "eof-after-exit" (equal r '(:ok ("last" :eof))) "~s" r))
    (kill-child proc))
  ;; The child exits while the read is blocked.
  (let* ((proc (child '(progn (sleep 2) (ccl:quit 0))))
         (out (ccl:external-process-output-stream proc))
         (job (start-job "eof reader"
                         (lambda () (read-line out nil :eof)))))
    (let ((r (wait-job job 30)))
      (check "eof-while-blocked" (equal r '(:ok :eof)) "~s" r))
    (kill-child proc)))

(defparameter *lines* 5000)

(defun expected-lines-p (lines)
  (and (= (length lines) *lines*)
       (loop for l in lines for i from 0
             always (equal l (princ-to-string i)))))

(defun case-output ()
  (let ((form `(progn (dotimes (i ,*lines*) (princ i) (terpri))
                      (finish-output) (ccl:quit 0))))
    ;; :output :stream
    (let* ((proc (child form))
           (out (ccl:external-process-output-stream proc))
           (job (start-job "output reader"
                           (lambda ()
                             (loop for l = (read-line out nil nil)
                                   while l collect l)))))
      (let ((r (wait-job job 60)))
        (check "output-stream-all-lines"
               (and (consp r) (eq (first r) :ok) (expected-lines-p (second r)))
               "~a" (if (consp r)
                      (if (eq (first r) :ok) (length (second r)) (second r))
                      r)))
      (kill-child proc))
    ;; :output to a lisp stream
    (let ((job (start-job "output to lisp stream"
                          (lambda ()
                            (with-output-to-string (s)
                              (ccl:run-program (ccl::kernel-path)
                                               (list "-I" ccl::*heap-image-name*
                                                     "-n" "-b" "-e"
                                                     (prin1-to-string form))
                                               :input nil :output s :wait t))))))
      (let ((r (wait-job job 60)))
        (check "output-lisp-stream-all-lines"
               (and (consp r) (eq (first r) :ok)
                    (expected-lines-p
                     (with-input-from-string (in (second r))
                       (loop for l = (read-line in nil nil)
                             while l collect (string-right-trim '(#\Return) l)))))
               "~a" (if (consp r) (if (eq (first r) :ok) (count #\Newline (second r)) (second r)) r))))))

(defun run-all ()
  (format t "~&LISP ~a~%KERNEL ~a~%" (lisp-implementation-version) (ccl::kernel-path))
  (finish-output)
  (dolist (c *cases*)
    (handler-case (funcall c)
      (error (e) (check (string-downcase (symbol-name c)) nil
                        "signalled ~a" (princ-to-string e)))))
  (format t "~&XT-RESULT WINDOWS-PIPE-READ TOTAL ~d FAILED ~d~%" *total* *failed*)
  (finish-output)
  ;; A thread stuck in a failed case must not keep the process alive.
  (ccl:external-call "ExitProcess"
                     :unsigned-fullword (if (zerop *failed*) 0 1)
                     :void))

(run-all)
