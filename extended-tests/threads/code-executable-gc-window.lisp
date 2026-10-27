;;; code-executable-gc-window.lisp -- a GC that finds a thread inside the
;;; c-frame build of %make-code-executable must find a well-formed stack.
;;;
;;; On arm64, %make-code-executable moved sp below its c-frame first and
;;; stored the frame header and saved sp in the next instructions.  A GC
;;; that stopped the thread in between walked the control stack from sp
;;; and read whatever an earlier call had left there.
;;;
;;; That window is a few instructions long, so a GC thread that races the
;;; caller almost never lands in it.  This test does not race.  It stops
;;; the caller thousands of times a second, reads the PC from its saved
;;; context, and runs a GC only while that PC is between the start of
;;; the frame build and the foreign call.  A correct lisp gets HITS such
;;; GCs and exits 0.  On a broken one the GC finds a bad word at sp and
;;; the kernel stops the process.
;;;
;;; The kernel stops the process only when stdin is not /dev/null, so
;;; the file runs the test in a child lisp with a pipe on its stdin, and
;;; kills the child if it is still running after *SECONDS* + 60.
;;;
;;; The test is for arm64 only; other targets report a skip.
;;;
;;; expected-state: clean
;;; expected-state linuxarm64: repro
;;;
;;; Contract: the child prints
;;;   CODE-EXECUTABLE-GC-WINDOW-RESULT :SAMPLES n :HITS n :BY-OFFSET v :VERDICT v
;;; and the parent echoes it, then prints its own verdict last:
;;;   CODE-EXECUTABLE-GC-WINDOW-VERDICT PASS|FAIL-...

(in-package :cl-user)

(defparameter *seconds* 30
  "The longest the test samples.")

(defparameter *hits-wanted* 2000
  "The test stops when this many GCs found the caller inside the frame build.")

(defvar *stop* nil)
(defvar *samples* 0)
(defvar *hits* 0)
(defvar *hits-by-offset* (make-array 16 :initial-element 0))

(defun report (verdict)
  (format t "~&CODE-EXECUTABLE-GC-WINDOW-RESULT :SAMPLES ~d :HITS ~d :BY-OFFSET ~s :VERDICT ~a~%"
          *samples* *hits* *hits-by-offset* verdict)
  (finish-output))

(unless (member :arm64-target *features*)
  (report "SKIP")
  (quit 0))

(defvar *child* nil
  "True in the child lisp, which the parent starts with this file.")

(unless *child*
  (let* ((file (namestring (truename *load-truename*)))
         (p (ccl:run-program (ccl::kernel-path)
                             (list "-I" ccl::*heap-image-name* "-n" "-b" "-Q"
                                   "-e" "(defvar cl-user::*child* t)" "-l" file)
                             :wait nil :input :stream :output :stream :error :output))
         (out (ccl:external-process-output-stream p)))
    (loop repeat (* 10 (+ *seconds* 60))
          while (eq (ccl:external-process-status p) :running)
          do (sleep 0.1))
    (when (eq (ccl:external-process-status p) :running)
      (ccl:signal-external-process p 9))
    (let ((lines (loop for line = (read-line out nil) while line collect line)))
      (multiple-value-bind (status code) (ccl:external-process-status p)
        (dolist (line lines)
          (when (or (search "RESULT" line) (search "Unexpected" line)
                    (search "Error" line))
            (format t "~&;; child: ~a~%" line)))
        (cond ((and (eq status :exited) (eql code 0)
                    (some (lambda (l) (search ":VERDICT PASS" l)) lines))
               (format t "~&CODE-EXECUTABLE-GC-WINDOW-VERDICT PASS~%")
               (finish-output) (quit 0))
              (t (format t "~&CODE-EXECUTABLE-GC-WINDOW-VERDICT FAIL-CHILD-~a-~a~%"
                         status code)
                 (finish-output) (quit 1)))))))

;;; Linux arm64 ucontext_t: uc_mcontext starts at byte 176, and in it
;;; fault_address and regs[0..30] come before sp and pc.
(defconstant +uc-pc+ (+ 176 8 (* 31 8) 8))

(defun frame-build-region ()
  "The code-vector byte range from `mov x1, sp' up to the first blr,
as absolute addresses."
  (let* ((cv (ccl::uvref #'ccl::%make-code-executable 0))
         (data (+ (logandc2 (ccl::%address-of cv) 15) 8))
         (n (ccl::uvsize cv))
         ;; A code vector is not a sequence, so walk it with uvref.
         (start (loop for i below n
                      when (= (ccl::uvref cv i) #x910003E1) return i))
         (end (and start
                   (loop for i from start below n
                         when (= (logand (ccl::uvref cv i) #xFFFFFC1F) #xD63F0000)
                           return i))))
    (unless (and start end (< start end n))
      (report "NO-FRAME-BUILD-FOUND")
      (quit 1))
    (values (+ data (* 4 start)) (+ data (* 4 end)))))

;;; A store that faults is slow, and a signal that arrives during the
;;; fault is taken with the PC still on the store.  So before each call
;;; the caller gives back the dead pages below its frame (they read as
;;; zero afterwards), then calls from a depth where the c-frame header
;;; store is the first write into one of those pages.

(defconstant +page+ 4096)
(defconstant +madv-dontneed+ 4)

(defun frame-address ()
  (ash (ccl::%get-frame-ptr) 3))

(defun at-depth (n v)
  ;; Not a tail call: each level leaves its frame on the stack.
  (if (zerop n)
      (progn (funcall 'ccl::%make-code-executable v) 0)
      (1+ (at-depth (1- n) v))))

(defun depth-address (n)
  (if (zerop n)
      (frame-address)
      (prog1 (depth-address (1- n)) (identity n))))

(defun zap-below (address)
  ;; Leave 2 pages for the foreign call to madvise itself.
  (let* ((hi (- (logandc2 address (1- +page+)) (* 2 +page+)))
         (lo (- hi (* 16 +page+))))
    (ccl::external-call "madvise" :address (ccl::%int-to-ptr lo)
                        :unsigned-doubleword (- hi lo)
                        :signed-fullword +madv-dontneed+ :signed-fullword)
    hi))

(defun caller ()
  (let* ((v (make-array 4 :element-type '(unsigned-byte 32) :initial-element 0))
         (top (frame-address))
         (step (max 1 (floor (- (depth-address 0) (depth-address 64)) 64)))
         ;; Depths that put the call between 2 and 4 pages below here.
         (d0 (floor (* 2 +page+) step))
         (dn (ceiling (* 2 +page+) step)))
    (format t "~&;; caller frame ~x, ~d bytes a level, depths ~d-~d~%"
            top step d0 (+ d0 dn))
    (finish-output)
    (loop for i from 0
          until *stop*
          do (zap-below (frame-address))
             (at-depth (+ d0 (mod i (1+ dn))) v))))

(defun suspended-pc (tcr)
  (let ((xp (ccl::%fixnum-ref-natural tcr arm64::tcr.suspend-context)))
    (and (/= xp 0)
         (ccl::%fixnum-ref-natural (ash xp -3) +uc-pc+))))

(multiple-value-bind (lo hi) (frame-build-region)
  (let* ((p (process-run-function "caller" #'caller))
         (deadline (+ (get-internal-real-time)
                      (* *seconds* internal-time-units-per-second))))
    (loop until (or (>= *hits* *hits-wanted*)
                    (> (get-internal-real-time) deadline))
          do (when (process-suspend p)
               (unwind-protect
                    (let ((pc (suspended-pc (ccl::process-tcr p))))
                      (incf *samples*)
                      (when (and pc (<= lo pc) (< pc hi))
                        (incf *hits*)
                        (incf (aref *hits-by-offset* (min 15 (ash (- pc lo) -2))))
                        (gc)))
                 (process-resume p))))
    (setq *stop* t)
    (process-wait "caller" #'process-exhausted-p p)
    (cond ((zerop *hits*) (report "NO-HITS") (quit 1))
          (t (report "PASS") (quit 0)))))
