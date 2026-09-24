;;; Reproducer for the suspend-vs-spinlock runtime deadlock (LISP-8 class).
;;;
;;; THE MECHANISM.  %SUSPEND-OTHER-THREADS is the one world-stop that returns
;;; with the world still stopped.  To RELEASE the exception lock afterwards it
;;; must take that lock's guard spin word.  If a second thread was suspended
;;; WHILE HOLDING that guard, the guard is never cleared and the release spins
;;; forever.  Nothing crashes and no error is reported, because the failure is
;;; in the error-delivery path.
;;;
;;; ⛔ FOUR THINGS THIS FILE MUST GET RIGHT.  The previous version got none of
;;; them and reported GREEN for an entire session (MEASURED 2026-09-10).
;;;
;;; 1. THE VICTIMS MUST BE COMPILED.  The deadlock needs a victim inside the
;;;    INLINE allocation trap -- the `sub %edx,%gs:0xd8 / cmp %gs:0xe0,%rbx /
;;;    ja' guard and then `int $0xc5' that every fleet capture shows.  That
;;;    sequence exists only in COMPILED code.  The old version built its
;;;    workers as anonymous lambdas inside a DEFVAR initform, which load as
;;;    INTERPRETED closures and allocate through runtime calls instead.  So
;;;    the worker body below is a top-level DEFUN, which CCL compiles.
;;;
;;; 2. THE VICTIMS MUST BE PARKABLE.  suspend_resume_handler only PARKS a
;;;    thread at TCR_INTERRUPT_LEVEL <= -2; `without-interrupts' binds -1,
;;;    which parks.  At level 0 the victim DEFERS, keeps running, and clears
;;;    the guard word, so the race cannot close however long the run is.
;;;
;;; 3. FOUR ALLOCATION SHAPES, NOT ONE.  The &rest path is what put
;;;    _SPheap_rest_arg in the 2A-FWA fleet backtrace.  A reproducer that
;;;    never applies a &rest list never exercises the frame the fleet died in.
;;;
;;; 4. THE EXPOSURE MUST EXCEED THE WINDOW.  MEASURED wedges on this hardware:
;;;    51,750 / 70,500 / 88,250 / 103,250 cycles.  The old default was 3,000
;;;    and its longest run was 25,000.  It could not have failed.
;;;
;;; Every run reports allocations and live worker count, because a worker set
;;; that died on its first iteration makes any run trivially green, and the
;;; old harness could not tell that apart from a clean pass.
;;;
;;; Environment:
;;;   REPRO_ITERS    world-stop iterations (default 500000)
;;;   REPRO_WORKERS  allocating worker threads (default 24)
;;;   REPRO_CELLS    base allocation size (default 20000)
;;;
;;; Output contract (the driver parses these):
;;;   REPRO-START ...                    before the first world-stop
;;;   REPRO-THREADS <n>                  live workers after settle
;;;   HEARTBEAT <i> allocs=<n> live=<n>  every 250 world-stops
;;;   REPRO-COMPLETE <i>                 only when every iteration finished

(in-package :cl-user)

(defun repro-envint (name default)
  (or (ignore-errors (parse-integer (ccl:getenv name))) default))

(defparameter *iters*    (repro-envint "REPRO_ITERS" 500000))
(defparameter *nworkers* (repro-envint "REPRO_WORKERS" 24))
(defparameter *cells*    (repro-envint "REPRO_CELLS" 20000))
(defparameter *allocs*   0)
(defparameter *stop*     nil)

;; A TOP-LEVEL DEFUN so CCL compiles it -- see note 1 above.  `kind' selects
;; one of four shapes; kind 3 is the &rest path that appears in the fleet
;; backtrace as _SPheap_rest_arg.
(defun repro-conser (kind n)
  (loop until *stop*
        do (ccl:without-interrupts
             (case kind
               (0 (make-list 500))
               (1 (make-array n))
               (2 (make-string (* 2 n)))
               (t (apply (lambda (&rest a) (length a))
                         (make-list 200 :initial-element 1)))))
           (ccl::atomic-incf *allocs*)))

(defparameter *workers*
  (loop for i below *nworkers*
        collect (let ((kind (mod i 4)) (n *cells*))
                  (ccl:process-run-function
                   (format nil "conser-~d" i)
                   (lambda () (repro-conser kind n))))))

(format t "~&REPRO-START workers=~d iters=~d cells=~d~%"
        *nworkers* *iters* *cells*)
(force-output)

(sleep 2)
(format t "~&REPRO-THREADS ~d~%"
        (count-if #'ccl::process-active-p *workers*))
(force-output)

(dotimes (i *iters*)
  ;; The RAW pair: %SUSPEND-OTHER-THREADS returns with the world still
  ;; stopped, and the deadlock is in the RELEASE that follows.  The
  ;; WITH-OTHER-THREADS-SUSPENDED macro pairs them differently and ran
  ;; 10,000 cycles green on a build that wedges at 88,250 with this.
  (ccl::%suspend-other-threads)
  (ccl::%resume-other-threads)
  (when (zerop (mod i 250))
    (format t "~&HEARTBEAT ~d allocs=~d live=~d~%"
            i *allocs*
            (count-if #'ccl::process-active-p *workers*))
    (force-output)))

(setq *stop* t)
(sleep 0.2)
(format t "~&REPRO-COMPLETE ~d~%" *iters*)
(force-output)
(ccl:quit 0)
