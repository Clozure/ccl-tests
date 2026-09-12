;;;; tools/beyond-ansi/negative-index-bound-check-test.lisp — GREEN-BAR.
;;;; Regression guard for the unsigned misc-ref/misc-set bound check
;;;; (upstream 72c714b3).  Fixed 2026-08-21; watched RED (12/8,
;;;; kernel e6296f7e, series minus 0219) then GREEN (12/0, kernel da242731,
;;;; full series) on the same image 9735b35d.  Do NOT edit the assertions.
;;;;
;;;; THE DEFECT (fixed by 0219).  The general array bound check on arm64
;;;; tested only the UPPER bound.  The four spentry bound checks (misc_ref,
;;;; subtag_misc_ref, subtag_misc_set, misc_set) translated PPC64 trlge, an
;;;; UNSIGNED trap, as the signed b.ge, so a negative index passed at safety 1
;;;; and the access ran below the vector data.  Measured on image
;;;; b1cd8ddc5bdce1f32554fe74a9067d73 with kernel
;;;; 4b36ba7f1ab681bbf2855c70a151a55d, from one compiled function
;;;;     (lambda (v i) (declare (optimize (safety 1) (speed 1))) (aref v i))
;;;;
;;;;     (aref u8-vector 99)      SIGNALLED "Invalid reference ... at index 99."
;;;;     (aref u8-vector -1)      0
;;;;     (aref simple-vector 99)  SIGNALLED, same shape
;;;;     (aref simple-vector -1)  #<BOGUS object @ #x4BE>
;;;;     (aref bit-vector -1)     0
;;;;     (aref double-float-v -1) 6.31D-321
;;;;     (aref string -1)         #\Null
;;;;     (uvref u8-vector -1)     0
;;;;     (row-major-aref sv -1)   #<BOGUS object @ #x4BE>
;;;;
;;;; The simple-vector case is the worst of these.  It hands a raw uvector
;;;; header word back to Lisp as an object, which is a garbage-collector hazard
;;;; and not merely a wrong number.
;;;;
;;;; ⛔ THIS FILE NEVER STORES.  The store side of the same defect CORRUPTS THE
;;;; HEAP.  Measured: an earlier version of the 0216 green-bar test ran
;;;; (setf (aref v -1) x) at the default policy, expecting a signal, and the
;;;; image died with
;;;;     GC: object 0x3020017edeac (dnode 0xe15e) claims 0x100000000000 suffix
;;;;     dnodes but the area has only 0x21374 - corrupt uvector header?
;;;; A test may not corrupt the image that runs it.  Add no store case here
;;;; until the check is fixed.
;;;;
;;;; WHERE THE DEFECT WAS NOT.  The check-misc-bound vinsn is correct.  It branches on b.lo, which is
;;;; unsigned, so a boxed -8 fails the test and traps.  SVREF proves that path
;;;; works: declared or not, SVREF open-codes and rejects -1.  The undeclared
;;;; AREF and UVREF forms do not open-code at all.  They call through rcontext,
;;;; at offset #x298 (.SPbuiltin-aref1) for AREF and #x468 (.SPmisc-ref) for
;;;; UVREF, which is how the miss localized to the four spentry sites.
;;;;
;;;; ORACLE, MEASURED 2026-08-21.  Stock x86-64 CCL (oracle binary
;;;; 6716f3c46b570f840a0aeb96c4111193) reports TOTAL 12 FAILED 0, rc=0.  The
;;;; defect was arm64-specific, and this file tests a gap in the standard,
;;;; not an implementation quirk (rule 1 of the beyond-ANSI bar).
;;;;
;;;; WHY ANSI CANNOT REACH THIS.  The standard leaves an out-of-bounds index
;;;; undefined, so no ANSI test can require a signal.  CCL's own contract at
;;;; safety 1 is to signal, and this file holds it to that contract.
;;;;
;;;; A RED here now is a REGRESSION of the 0219 bound check.

(load (merge-pathnames "../harness.lisp" *load-pathname*))
(in-package :cl-user)

(defparameter *nbc-aref*
  (compile nil '(lambda (v i)
                  (declare (optimize (safety 1) (speed 1)))
                  (aref v i))))

(defparameter *nbc-uvref*
  (compile nil '(lambda (v i)
                  (declare (optimize (safety 1) (speed 1)))
                  (ccl::uvref v i))))

(defparameter *nbc-rma*
  (compile nil '(lambda (v i)
                  (declare (optimize (safety 1) (speed 1)))
                  (row-major-aref v i))))

(defun nbc-signals-p (fn v i)
  "T when (FN V I) signals.  The value is DISCARDED without printing it.
A returned value here is an out-of-bounds read, and for a gvector it can be a
bogus object whose printer would fault.  So the case reports a boolean and
never the object."
  (handler-case (progn (funcall fn v i) nil)
    (serious-condition () t)))

(defparameter *nbc-u8*
  (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(7 8 9 10)))
(defparameter *nbc-sv* (vector 7 8 9 10))
(defparameter *nbc-bit* (make-array 4 :element-type 'bit :initial-element 1))
(defparameter *nbc-df*
  (make-array 4 :element-type 'double-float :initial-element 1d0))
(defparameter *nbc-str* (make-string 4 :initial-element #\a))

;;; The positive-bound cases are the POSITIVE CONTROL for this file.  They must
;;; pass.  If they ever fail, the check is not merely one-sided, it is absent,
;;; and this file would otherwise report that as the same red.
(xt-check "nbc.control.aref-u8-above-bound-signals"
          (nbc-signals-p *nbc-aref* *nbc-u8* 99) t)
(xt-check "nbc.control.aref-simplevec-above-bound-signals"
          (nbc-signals-p *nbc-aref* *nbc-sv* 99) t)
(xt-check "nbc.control.uvref-u8-above-bound-signals"
          (nbc-signals-p *nbc-uvref* *nbc-u8* 99) t)

;;; The defect.  Every one of these is EXPECTED RED on arm64 today.
(xt-check "nbc.aref-u8-negative-signals"
          (nbc-signals-p *nbc-aref* *nbc-u8* -1) t)
(xt-check "nbc.aref-simplevec-negative-signals"
          (nbc-signals-p *nbc-aref* *nbc-sv* -1) t)
(xt-check "nbc.aref-bit-negative-signals"
          (nbc-signals-p *nbc-aref* *nbc-bit* -1) t)
(xt-check "nbc.aref-double-float-negative-signals"
          (nbc-signals-p *nbc-aref* *nbc-df* -1) t)
(xt-check "nbc.aref-string-negative-signals"
          (nbc-signals-p *nbc-aref* *nbc-str* -1) t)
(xt-check "nbc.uvref-u8-negative-signals"
          (nbc-signals-p *nbc-uvref* *nbc-u8* -1) t)
(xt-check "nbc.uvref-simplevec-negative-signals"
          (nbc-signals-p *nbc-uvref* *nbc-sv* -1) t)
(xt-check "nbc.row-major-aref-simplevec-negative-signals"
          (nbc-signals-p *nbc-rma* *nbc-sv* -1) t)

;;; SVREF is the counter-example that localizes the defect, so it belongs in the
;;; same file.  It must PASS on arm64 today, which makes it a second positive
;;; control: the port already contains a correct one-instruction check.
(xt-check "nbc.control.svref-negative-signals"
          (nbc-signals-p
           (compile nil '(lambda (v i)
                           (declare (type simple-vector v)
                                    (optimize (safety 1) (speed 1)))
                           (svref v i)))
           *nbc-sv* -1)
          t)

(xt-report "NEGATIVE-INDEX-BOUND-CHECK")
