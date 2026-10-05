;;;; windows-pipe-latency.lisp -- how long a run-program pipe read waits
;;;; after the data arrives, on Windows
;;;;
;;;; Run it in its own process, under an external timeout:
;;;;
;;;;     wx86cl64 -n -b -l windows-pipe-latency.lisp
;;;;
;;;; The child is a second copy of the same lisp.  It writes N lines to
;;;; its standard output, at random intervals of 20 to 50 ms, and each line
;;;; carries the value of QueryPerformanceCounter at the moment of the
;;;; write.  The parent reads the lines and takes the counter again when
;;;; each read returns.  That counter is system-wide, so the difference is
;;;; the time that a line waited in the pipe before the read returned it.
;;;;
;;;; The child spins on the counter between lines and does not call SLEEP,
;;;; because a sleep ends on a timer tick and a polling reader also wakes
;;;; on a tick.  The two would line up, and the wait would not show.
;;;;
;;;; A reader that polls the pipe and sleeps between polls waits up to one
;;;; timer tick (15.6 ms by default) for each line, about half a tick on
;;;; average.  A reader that blocks returns within a fraction of a
;;;; millisecond.  The case passes when the median wait is under 3 ms.
;;;;
;;;; The last line is
;;;;     XT-RESULT WINDOWS-PIPE-LATENCY TOTAL <n> FAILED <n>
;;;; and the exit status is 0 only if FAILED is 0.  On any other platform
;;;; the case is a SKIP.

(in-package :cl-user)

(load (merge-pathnames "../harness.lisp" *load-pathname*))

(defparameter *lines* 60)
(defparameter *median-limit-ms* 3)

(defun qpc-counter ()
  (ccl::%stack-block ((buf 8))
    (ccl:external-call "QueryPerformanceCounter" :address buf :int)
    (ccl::%get-signed-long-long buf 0)))

(defun qpc-frequency ()
  (ccl::%stack-block ((buf 8))
    (ccl:external-call "QueryPerformanceFrequency" :address buf :int)
    (ccl::%get-signed-long-long buf 0)))

(defparameter *child-form*
  `(let ((freq (ccl::%stack-block ((buf 8))
                 (ccl:external-call "QueryPerformanceFrequency" :address buf :int)
                 (ccl::%get-signed-long-long buf 0)))
         (state (make-random-state t)))
     (flet ((now ()
              (ccl::%stack-block ((buf 8))
                (ccl:external-call "QueryPerformanceCounter" :address buf :int)
                (ccl::%get-signed-long-long buf 0))))
       (dotimes (i ,*lines*)
         (let ((until (+ (now) (floor (* freq (+ 20 (random 30 state))) 1000))))
           (loop while (< (now) until)))
         (format t "~d~%" (now))
         (finish-output))
       (ccl:quit 0))))

(defun run-child ()
  (ccl:run-program (ccl::kernel-path)
                   (list "-I" ccl::*heap-image-name* "-n" "-b"
                         "-e" (let ((*package* (find-package :cl-user))
                                    (*print-readably* nil))
                                (prin1-to-string *child-form*)))
                   :input nil :output :stream :error :output
                   :wait nil))

(defun run-case ()
  (let* ((freq (qpc-frequency))
         (proc (run-child))
         (out (ccl:external-process-output-stream proc))
         (waits '())
         (bad 0))
    (loop
      (let ((line (read-line out nil nil)))
        (unless line (return))
        (let* ((got (qpc-counter))
               (sent (ignore-errors (parse-integer (string-trim '(#\Return) line)))))
          (if sent
            (push (/ (* (- got sent) 1000.0d0) freq) waits)
            (incf bad)))))
    ;; The first lines also wait for the child to start.  Drop them.
    (let* ((all (nreverse waits))
           (kept (sort (coerce (nthcdr 3 all) 'vector) #'<))
           (n (length kept)))
      (when (< n 10)
        (error "only ~d timed lines (~d unparsed)" n bad))
      (format t "~&LATENCY lines ~d median-ms ~,3f p90-ms ~,3f max-ms ~,3f~%"
              n (aref kept (floor n 2)) (aref kept (floor (* n 9) 10))
              (aref kept (1- n)))
      (finish-output)
      (aref kept (floor n 2)))))

(format t "~&LISP ~a~%KERNEL ~a~%" (lisp-implementation-version) (ccl::kernel-path))
(finish-output)
#+windows-target
(xt-true "pipe-read-latency-median-under-3-ms"
         (< (run-case) *median-limit-ms*))
#-windows-target
(xt-skip "pipe-read-latency-median-under-3-ms" "Windows only")
(xt-report "WINDOWS-PIPE-LATENCY")
