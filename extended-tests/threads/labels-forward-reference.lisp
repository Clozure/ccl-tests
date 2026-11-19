;;; Reproducer: the closures of a LABELS are linked to each other
;;; without the EGC write barrier.  Tracks Clozure/ccl issue #726 and
;;; PR #727.
;;;
;;; The compiler allocates the closures of a LABELS one after another,
;;; each with NIL in the cells for siblings that do not exist yet, and
;;; then stores those siblings into it.  The stores were plain stores
;;; (the SET-CLOSURE-FORWARD-REFERENCE vinsn on x86 and arm64,
;;; MISC-SET-C-NODE on arm) on the reasoning that they initialize a new
;;; object.  They do not: a GC can run between the allocation of one
;;; closure and that of a sibling (another thread's GC, or this
;;; thread's own at one of the later allocations).  The earlier closure
;;; then survives into an older generation, the later one is young, and
;;; the store makes an old-to-young reference that is not in the
;;; refbits.  At the next ephemeral GC the young closure (still
;;; referenced from the stack) is moved, and the old closure keeps its
;;; former address.
;;;
;;; The class is the GC, but it lives in threads/ because it needs
;;; threads to be quick and because it can hang (rule 5).
;;;
;;; LABELS-FWD-REF-MAKE-RING is one function with a LABELS of 200
;;; closures, each of which refers to the next (the last to the first)
;;; and to the argument.  Each worker makes a ring, allocates a little
;;; so that GCs keep happening, and walks the ring twice round, checking
;;; that each closure answers a function and the argument, and that the
;;; walk is back at the first closure after every 200 steps.  A run ends
;;; one of three ways when the defect is present:
;;;   - a closure answers something else (another closure of the ring,
;;;     another thread's closure, NIL, a bogus object): the vacated
;;;     memory is in use again;
;;;   - an error in a worker, usually "Fault during write to memory
;;;     address #xF" on x86-64 (a call through the stale pointer into
;;;     vacated, zeroed memory), once "Stack overflow on value stack";
;;;   - a hang, with one thread busy in the GC: the GC followed a stale
;;;     pointer while marking a stack.  The runner's timeout reports it.
;;; Either of the first two exits 42.  A fault the lisp kernel cannot
;;; hand to Lisp ends the process with another non-zero status, which
;;; the runner also counts as a reproduction.
;;;
;;; Watched red and green, linuxx8664, 16 threads (the default) on 2
;;; CPUs, REPRO_SECONDS=20: master 23c9edcf exited 42 in 7 runs of 7,
;;; within 10 s, printing for example
;;;   LABELS-FWD-REF: thread 1: closure 153 of a ring answered
;;;   #<COMPILED-LEXICAL-CLOSURE (:INTERNAL F92 LABELS-FWD-REF-MAKE-RING) ...> and 7
;;;   LABELS-FWD-REF: Fault during write to memory address #xF in thread 0 after 778 rings
;;; and master with the fix ran clean 5 of 5.  The same
;;; workload as a standalone file failed 3 runs of 3 on each of the
;;; released 1.12.2, 1.13 and 1.14 and 3 of 3 on linuxx8632 master, ran
;;; clean with the fix on linuxx8632 (5 of 5, and once for 120 s), and
;;; did not fail with the ephemeral GC off.  Unpinned on 22 CPUs, 16
;;; threads still failed twice in two 30 s runs; 88 threads did not.
;;;
;;; Environment: REPRO_THREADS (16), REPRO_SECONDS (60), REPRO_EGC (1;
;;; 0 turns the ephemeral GC off).
;;; Result line: LABELS-FWD-REF-RESULT :RINGS n :SECONDS s
;;;
;;; Fixed by 66d5d5c4, the merge of PR #727.  At that commit
;;; (v1.14-51-g66d5d5c4) on darwinarm64, 12 CPUs, the defaults ran
;;; clean 3 of 3, about 10 million rings each.
;;;
;;; On solarisx8664 and solarisx8632, 8 CPUs, at v1.14-57-g62df3a686,
;;; the defaults ran clean (4.7 and 5.5 million rings).  With the x86
;;; compiler of 66d5d5c4^ loaded into those same lisps (the vinsns
;;; file and x862.lisp, with *WARN-IF-REDEFINE-KERNEL* nil), the
;;; closures were again linked with a plain store instead of .SPgvset, and
;;; the defaults exited 42 in 3 runs of 3 on each, within 15 s: on
;;; x8664 always "Fault during write to memory address #xF", on x8632
;;; twice a wrong answer and once "#<COMPILED-LEXICAL-CLOSURE ...> is
;;; not of type (OR SYMBOL FUNCTION)".
;;;
;;; expected-state: clean

(in-package :cl-user)

(defun labels-fwd-ref-env (name default)
  (or (ignore-errors (parse-integer (ccl:getenv name))) default))

;;; At least one worker for at least one second: with none, the run
;;; would test nothing and still report clean.
(defvar *labels-fwd-ref-threads* (max 1 (labels-fwd-ref-env "REPRO_THREADS" 16)))
(defvar *labels-fwd-ref-seconds* (max 1 (labels-fwd-ref-env "REPRO_SECONDS" 60)))
(defvar *labels-fwd-ref-counts* (make-array *labels-fwd-ref-threads* :initial-element 0))
(defvar *labels-fwd-ref-stop* nil)
(defvar *labels-fwd-ref-done* (ccl:make-semaphore))

(defun labels-fwd-ref-exit (code)
  (finish-output)
  ;; Not QUIT: after the defect has shown itself the heap is not to be
  ;; trusted, and QUIT has other threads to stop.
  (ccl:external-call "_exit" :int code))

(defmacro define-labels-ring (name n)
  (let ((names (loop for i below n collect (intern (format nil "F~d" i)))))
    `(defun ,name (x)
       (labels ,(loop for i below n
                      collect `(,(nth i names) ()
                                (values #',(nth (mod (1+ i) n) names) x)))
         #',(first names)))))

(define-labels-ring labels-fwd-ref-make-ring 200)

(defun labels-fwd-ref-rings () (reduce #'+ *labels-fwd-ref-counts*))

(defun labels-fwd-ref-worker (id)
  ;; A memory fault in a worker is signalled as an error in that thread.
  ;; Handle it here: under --batch the debugger is not reached.  The
  ;; handler prints the fixed part of its line first, and prints the
  ;; condition under a handler of its own: in a damaged heap, printing
  ;; it can fault again, and that must not stop the exit with 42.
  (handler-bind ((serious-condition
                   (lambda (c)
                     (format t "~&LABELS-FWD-REF: thread ~d after ~d rings: "
                             id (labels-fwd-ref-rings))
                     (finish-output)
                     (handler-case (format t "~a~%" c)
                       (serious-condition ()
                         (format t "(condition could not be printed)~%")))
                     (labels-fwd-ref-exit 42))))
    (labels-fwd-ref-loop id))
  (ccl:signal-semaphore *labels-fwd-ref-done*))

(defun labels-fwd-ref-loop (id)
  (loop until *labels-fwd-ref-stop*
        do (let* ((start (labels-fwd-ref-make-ring id))
                  (f start))
             (dotimes (k 3) (make-array 32768 :element-type '(unsigned-byte 8)))
             (dotimes (i 400)
               (multiple-value-bind (next x) (funcall f)
                 (unless (and (eql x id) (functionp next))
                   (format t "~&LABELS-FWD-REF: thread ~d: closure ~d of a ring answered ~s and ~s~%"
                           id (mod i 200) next x)
                   (labels-fwd-ref-exit 42))
                 ;; A stale pointer to another closure (of this ring or
                 ;; an older one) also answers a function and the
                 ;; argument, but the walk then does not close after 200.
                 (when (and (= (mod i 200) 199) (not (eq next start)))
                   (format t "~&LABELS-FWD-REF: thread ~d: a ring did not close after ~d steps; at ~s~%"
                           id (1+ i) next)
                   (labels-fwd-ref-exit 42))
                 (setq f next)))
             (incf (svref *labels-fwd-ref-counts* id)))))

(when (eql 0 (labels-fwd-ref-env "REPRO_EGC" 1))
  (ccl:egc nil))

(format t "~&REPRO-START ~a; threads=~d seconds=~d egc=~:[off~;on~]~%"
        (lisp-implementation-version) *labels-fwd-ref-threads*
        *labels-fwd-ref-seconds* (ccl:egc-enabled-p))
(finish-output)

(dotimes (i *labels-fwd-ref-threads*)
  (let ((i i))
    (ccl:process-run-function (format nil "labels-fwd-ref-~d" i)
                              (lambda () (labels-fwd-ref-worker i)))))

(loop with elapsed = 0
      while (< elapsed *labels-fwd-ref-seconds*)
      do (let ((s (min 5 (- *labels-fwd-ref-seconds* elapsed))))
           (sleep s)
           (incf elapsed s))
         (format t "~&HEARTBEAT ~d s, ~d rings~%" elapsed (labels-fwd-ref-rings))
         (finish-output))

;;; Stop the workers and wait for each to finish its ring before saying
;;; clean.  A worker that finds the defect exits 42 instead, so the
;;; process cannot exit 0 while a worker is reporting a failure.
(setq *labels-fwd-ref-stop* t)
(dotimes (i *labels-fwd-ref-threads*)
  (ccl:wait-on-semaphore *labels-fwd-ref-done*))

(format t "~&LABELS-FWD-REF-RESULT :RINGS ~d :SECONDS ~d~%"
        (labels-fwd-ref-rings) *labels-fwd-ref-seconds*)
(format t "~&REPRO-COMPLETE~%")
(labels-fwd-ref-exit 0)
