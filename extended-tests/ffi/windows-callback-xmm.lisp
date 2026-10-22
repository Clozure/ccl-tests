;;;; windows-callback-xmm.lisp -- a callback keeps the caller's xmm6-xmm15,
;;;; on Windows x86-64
;;;;
;;;; The Win64 calling convention makes xmm6-xmm15 callee-saved, so a C
;;;; function that calls a Lisp callback may keep values in those registers
;;;; across the call.  Clozure/ccl issue #657: a callback returned with xmm15
;;;; set to 0.0.
;;;;
;;;; There is no C compiler in the test path, so the C side is a small
;;;; machine-code stub, copied into executable memory.  Its source:
;;;;
;;;;     xmm_across(f, in, out)      ; rcx = f, rdx = in, r8 = out
;;;;         push rbx; push rsi; sub rsp, 0xc8
;;;;         mov rbx, rdx; mov rsi, r8
;;;;         save xmm6-xmm15 at rsp+0x20 .. rsp+0xbf
;;;;         movsd xmm6..xmm15 <- in[0..9]
;;;;         call f
;;;;         movsd out[0..9] <- xmm6..xmm15
;;;;         restore xmm6-xmm15; add rsp, 0xc8; pop rsi; pop rbx; ret
;;;;
;;;; The callback does double-float arithmetic, so the lisp uses its own xmm
;;;; registers while it runs.  The case passes when OUT equals IN.
;;;;
;;;; Watched red and green on Windows 11 x86-64, each lisp built with
;;;; (rebuild-ccl :full t), 3 runs each:
;;;;     master c55fcb20 (before 32c3f8e1): FAIL, every run printed
;;;;         XMM-MISMATCH xmm15 sent 1155.75 received 0.0
;;;;     master a6588c23 (with 32c3f8e1):   PASS
;;;;
;;;; The last line is
;;;;     XT-RESULT WINDOWS-CALLBACK-XMM TOTAL <n> FAILED <n>
;;;; On any other platform the case is a SKIP.

(in-package :cl-user)

(load (merge-pathnames "../harness.lisp" *load-pathname*))

(defparameter *xmm-across-code*
  #(#x53 #x56 #x48 #x81 #xec #xc8 #x00 #x00 #x00 #x48 #x89 #xd3
    #x4c #x89 #xc6 #x0f #x11 #x74 #x24 #x20 #x0f #x11 #x7c #x24
    #x30 #x44 #x0f #x11 #x44 #x24 #x40 #x44 #x0f #x11 #x4c #x24
    #x50 #x44 #x0f #x11 #x54 #x24 #x60 #x44 #x0f #x11 #x5c #x24
    #x70 #x44 #x0f #x11 #xa4 #x24 #x80 #x00 #x00 #x00 #x44 #x0f
    #x11 #xac #x24 #x90 #x00 #x00 #x00 #x44 #x0f #x11 #xb4 #x24
    #xa0 #x00 #x00 #x00 #x44 #x0f #x11 #xbc #x24 #xb0 #x00 #x00
    #x00 #xf2 #x0f #x10 #x33 #xf2 #x0f #x10 #x7b #x08 #xf2 #x44
    #x0f #x10 #x43 #x10 #xf2 #x44 #x0f #x10 #x4b #x18 #xf2 #x44
    #x0f #x10 #x53 #x20 #xf2 #x44 #x0f #x10 #x5b #x28 #xf2 #x44
    #x0f #x10 #x63 #x30 #xf2 #x44 #x0f #x10 #x6b #x38 #xf2 #x44
    #x0f #x10 #x73 #x40 #xf2 #x44 #x0f #x10 #x7b #x48 #xff #xd1
    #xf2 #x0f #x11 #x36 #xf2 #x0f #x11 #x7e #x08 #xf2 #x44 #x0f
    #x11 #x46 #x10 #xf2 #x44 #x0f #x11 #x4e #x18 #xf2 #x44 #x0f
    #x11 #x56 #x20 #xf2 #x44 #x0f #x11 #x5e #x28 #xf2 #x44 #x0f
    #x11 #x66 #x30 #xf2 #x44 #x0f #x11 #x6e #x38 #xf2 #x44 #x0f
    #x11 #x76 #x40 #xf2 #x44 #x0f #x11 #x7e #x48 #x0f #x10 #x74
    #x24 #x20 #x0f #x10 #x7c #x24 #x30 #x44 #x0f #x10 #x44 #x24
    #x40 #x44 #x0f #x10 #x4c #x24 #x50 #x44 #x0f #x10 #x54 #x24
    #x60 #x44 #x0f #x10 #x5c #x24 #x70 #x44 #x0f #x10 #xa4 #x24
    #x80 #x00 #x00 #x00 #x44 #x0f #x10 #xac #x24 #x90 #x00 #x00
    #x00 #x44 #x0f #x10 #xb4 #x24 #xa0 #x00 #x00 #x00 #x44 #x0f
    #x10 #xbc #x24 #xb0 #x00 #x00 #x00 #x48 #x81 #xc4 #xc8 #x00
    #x00 #x00 #x5e #x5b #xc3))

(defvar *xmm-sink* 0d0)

#+windows-target
(ccl:defcallback xmm-busy-callback (:void)
  (let ((s 0d0))
    (dotimes (i 200)
      (setq s (+ (* s 0.5d0) (sqrt (+ i 1.25d0)))))
    (setq *xmm-sink* s)))

#+windows-target
(defun xmm-across-stub ()
  "Executable memory holding *XMM-ACROSS-CODE*."
  (let ((p (ccl:external-call "VirtualAlloc"
                              :address (ccl:%null-ptr)
                              :unsigned-doubleword 4096
                              :unsigned-fullword #x3000   ; MEM_COMMIT|MEM_RESERVE
                              :unsigned-fullword #x40     ; PAGE_EXECUTE_READWRITE
                              :address)))
    (when (ccl:%null-ptr-p p)
      (error "VirtualAlloc failed"))
    (dotimes (i (length *xmm-across-code*) p)
      (setf (ccl:%get-unsigned-byte p i) (aref *xmm-across-code* i)))))

#+windows-target
(defun xmm-callback-mismatches ()
  "Return a list of (register sent received) for each register that changed."
  (let ((stub (xmm-across-stub))
        (bad '()))
    (ccl:%stack-block ((in 80) (out 80))
      (dotimes (i 10)
        (setf (ccl:%get-double-float in (* 8 i)) (+ 1000.5d0 (* i 17.25d0))
              (ccl:%get-double-float out (* 8 i)) -1d0))
      (ccl:ff-call stub :address xmm-busy-callback :address in :address out :void)
      (dotimes (i 10)
        (let ((sent (ccl:%get-double-float in (* 8 i)))
              (got (ccl:%get-double-float out (* 8 i))))
          (unless (= sent got)
            (push (list (format nil "xmm~d" (+ i 6)) sent got) bad)))))
    (dolist (b (reverse bad))
      (format t "~&XMM-MISMATCH ~a sent ~f received ~f~%" (first b) (second b) (third b)))
    (finish-output)
    bad))

#+windows-target
(xt-true "callback-preserves-xmm6-to-xmm15"
         (null (xmm-callback-mismatches)))
#-windows-target
(xt-skip "callback-preserves-xmm6-to-xmm15" "Windows x86-64 only")
(xt-report "WINDOWS-CALLBACK-XMM")
