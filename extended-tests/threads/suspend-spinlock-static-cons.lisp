;;; Lisp-side reproducer for the suspend-vs-spin-word deadlock
;;; (upstream issue #597, PR #634) -- the EXCEPTION LOCK route.
;;;
;;; This is the Lisp counterpart of suspend-spinlock-deadlock.lisp.
;;; That one wedges the C route: an allocation trap takes EXCEPTION_LOCK
;;; from inside an exception handler.  This one wedges the same lock
;;; from ordinary Lisp code, with no exception involved.
;;;
;;; Vehicle: STATIC-CONS.  Every call takes the exception lock --
;;;
;;;    (defun static-cons (car-value cdr-value)
;;;      ...
;;;      (loop (with-exception-lock (without-interrupts ...))))
;;;
;;; -- and *KERNEL-EXCEPTION-LOCK* is the same memory as the kernel's
;;; EXCEPTION_LOCK (see DEF-CCL-POINTERS KERNEL-LOCKS in l1-aprims.lisp).
;;; One structure, two acquire paths, one guard word.
;;;
;;; The wedge: a worker is suspended while holding that guard word.  The
;;; thread that stopped the world needs the same word to release
;;; EXCEPTION_LOCK on its way out of the XUUO_SUSPEND_ALL handler
;;; (unlock_exception_lock_in_handler), so it spins in get_spin_lock
;;; while the only thread that could release the word is frozen.  One
;;; core at 100%, no crash, no debugger entry -- and a C backtrace that
;;; should look exactly like the one suspend-spinlock-deadlock.lisp
;;; produces, because past the guard word it IS the same failure.
;;;
;;; Why this file and not suspend-spinlock-rwlock.lisp: that one takes a
;;; user RWLOCK inside WITH-OTHER-THREADS-SUSPENDED.  No rwlock is
;;; aliased onto a kernel lock and nothing in the tree takes a Lisp lock
;;; inside a world-stop, so it reproduces a hazard the shipping system
;;; does not have.  This file uses a lock the runtime itself holds while
;;; the world is stopped, on an allocation path any program can reach.
;;;
;;; Fixed builds take the exception lock's guard word with suspension
;;; deferred (*INTERRUPT-LEVEL* -2) across the guard-word critical
;;; section only, so the worker cannot be frozen holding it.
;;;
;;; Environment:
;;;   REPRO_ITERS        world-stop iterations (default 500000)
;;;   REPRO_WORKERS      static-consing worker threads (default 24)
;;;   REPRO_LISP_WIDEN   widen the Lisp spin-word window, if the build
;;;                      carries ../wideners/widen-lisp-spin-release.patch
;;;
;;; Output contract (the driver parses these):
;;;   REPRO-START ...                    before the first world-stop
;;;   REPRO-THREADS <n>                  live workers after settle
;;;   HEARTBEAT <i> allocs=<n> live=<n>  every 250 world-stops
;;;   REPRO-COMPLETE <i>                 only when every iteration finished
;;;
;;; RED_FLOOR: SET FROM MEASUREMENT, NOT JUDGEMENT.  Reds measured
;;; 2026-09-16, 24 workers, unwidened, on kernels that already carry the
;;; C-side EXCEPTION_LOCK conversion -- so what wedges is the Lisp
;;; acquire and nothing else:
;;;
;;;   darwinarm64   cycle 0     (6,464,835 static conses)
;;;   darwinarm64   cycle 0     (6,661,596)
;;;   linuxarm64    cycle 750   (9,674,782)  + C backtrace, see above
;;;
;;; The matching greens ran the full 500000 cycles: 446,276,091 static
;;; conses on darwinarm64, 451,832,386 on linuxarm64.
;;;
;;; This reproducer wedges far sooner and far more tightly than
;;; suspend-spinlock-deadlock.lisp, whose reds span 18,500 to 200,250 --
;;; every STATIC-CONS takes the lock, so 24 threads doing nothing else
;;; keep the guard word occupied a large fraction of the time instead of
;;; waiting for an allocation trap to fire.  The driver's 400000 default
;;; is therefore ~533x the longest red seen here: conservative on
;;; purpose, and fine to keep.
;;;
;;; Three reds is a thin sample next to that file's ten.  Raise the
;;; floor, never lower it, and only against measured reds.

(in-package :cl-user)

(defun repro-envint (name default)
  (or (ignore-errors (parse-integer (ccl:getenv name))) default))

(defparameter *iters*    (repro-envint "REPRO_ITERS" 500000))
(defparameter *nworkers* (repro-envint "REPRO_WORKERS" 24))
(defparameter *allocs*   0)
(defparameter *stop*     nil)

;;; Armed at run time, like suspend-spinlock-rwlock.lisp: on a build
;;; without the widener the variable is absent and this file still runs
;;; unwidened, which is the only mode that can support a claim about a
;;; shipping build.
(defparameter *lisp-widen*
  (let ((n (repro-envint "REPRO_LISP_WIDEN" 0))
        (sym (find-symbol "*SPIN-RELEASE-WIDEN-LOOPS*" :ccl)))
    (if (and sym (boundp sym))
      (progn (when (> n 0) (set sym n)) n)
      :absent)))

;;; A TOP-LEVEL DEFUN so CCL compiles it rather than interpreting it:
;;; an interpreted closure would allocate through runtime calls and take
;;; a different path into the lock.
;;;
;;; The cons is dropped on the floor deliberately.  Static conses are
;;; collected, they just don't move, so discarding keeps the free list
;;; churning: workers pop it, the GC's reclaim_static_dnodes rebuilds it
;;; wholesale, and an empty list traps to the kernel through
;;; %ENSURE-STATIC-CONSES.  All three mutators are live, which is the
;;; situation the exception lock is there to arbitrate.
(defun repro-static-conser ()
  (loop until *stop*
        do (ccl:static-cons 0 0)
           (ccl::atomic-incf *allocs*)))

(defparameter *workers*
  (loop for i below *nworkers*
        collect (ccl:process-run-function
                 (format nil "static-conser-~d" i)
                 #'repro-static-conser)))

(format t "~&REPRO-START workers=~d iters=~d lisp-widen=~a free-static=~d~%"
        *nworkers* *iters* *lisp-widen* (ccl:free-static-conses))
(force-output)

(sleep 2)
(format t "~&REPRO-THREADS ~d~%"
        (count-if #'ccl::process-active-p *workers*))
(force-output)

(dotimes (i *iters*)
  ;; The RAW pair, matching suspend-spinlock-deadlock.lisp:
  ;; %SUSPEND-OTHER-THREADS returns with the world still stopped, which
  ;; is what puts the stopper on the exception lock's guard word while
  ;; the workers are frozen.  WITH-OTHER-THREADS-SUSPENDED pairs them
  ;; differently and is much weaker at provoking this.
  (ccl::%suspend-other-threads)
  (ccl::%resume-other-threads)
  (when (zerop (mod i 250))
    (format t "~&HEARTBEAT ~d allocs=~d live=~d free=~d~%"
            i *allocs*
            (count-if #'ccl::process-active-p *workers*)
            (ccl:free-static-conses))
    (force-output)
    ;; Collect, or the run dies to the OOM killer instead of reporting a
    ;; verdict.  Static conses come back only through the GC --
    ;; reclaim_static_dnodes rebuilds the free list wholesale -- while
    ;; %ENSURE-STATIC-CONSES only ever extends the area.  These workers
    ;; touch nothing but static conses, so nothing here would trigger a
    ;; collection on its own and the static area grows without bound:
    ;; measured at roughly 8 MB/s with 8 workers, and a 500000-cycle run
    ;; was SIGKILLed at 518M conses (rc=137, which the driver reports as
    ;; VERDICT ERROR, not RED).
    ;;
    ;; This is not just housekeeping.  It puts the third mutator in play
    ;; deliberately: the GC rebuilding the free list, the workers popping
    ;; it, and %ENSURE-STATIC-CONSES extending it without stopping the
    ;; world -- which is the three-way contention the exception lock is
    ;; there to arbitrate.  The free= field is printed before the
    ;; collection so it shows how far down the list was drawn.
    (ccl:gc)))

(setq *stop* t)
(sleep 0.2)
(format t "~&REPRO-COMPLETE ~d~%" *iters*)
(force-output)
(ccl:quit 0)
