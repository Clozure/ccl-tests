;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 19:13:34 2003
;;;; Contains: Tests of BIT-IOR

(in-package :cl-test)

(compile-and-load "bit-aux.lsp")

(deftest bit-ior.1
  (let* ((s1 (make-array nil :initial-element 0 :element-type 'bit))
	 (s2 (make-array nil :initial-element 0 :element-type 'bit)))
    (values (bit-ior s1 s2) s1 s2))
  #0a0
  #0a0
  #0a0)

(deftest bit-ior.2
  (let* ((s1 (make-array nil :initial-element 1 :element-type 'bit))
	 (s2 (make-array nil :initial-element 0 :element-type 'bit)))
    (values (bit-ior s1 s2) s1 s2))
  #0a1
  #0a1
  #0a0)

(deftest bit-ior.3
  (let* ((s1 (make-array nil :initial-element 0 :element-type 'bit))
	 (s2 (make-array nil :initial-element 1 :element-type 'bit)))
    (values (bit-ior s1 s2) s1 s2))
  #0a1
  #0a0
  #0a1)

(deftest bit-ior.4
  (let* ((s1 (make-array nil :initial-element 1 :element-type 'bit))
	 (s2 (make-array nil :initial-element 1 :element-type 'bit)))
    (values (bit-ior s1 s2) s1 s2))
  #0a1
  #0a1
  #0a1)

(deftest bit-ior.5
  (let* ((s1 (make-array nil :initial-element 0 :element-type 'bit))
	 (s2 (make-array nil :initial-element 0 :element-type 'bit))
	 (s3 (make-array nil :initial-element 1 :element-type 'bit))
	 (result (bit-ior s1 s2 s3)))
    (values s1 s2 s3 result (eqt s3 result)))
  #0a0
  #0a0
  #0a0
  #0a0
  t)

(deftest bit-ior.6
  (let* ((s1 (make-array nil :initial-element 1 :element-type 'bit))
	 (s2 (make-array nil :initial-element 1 :element-type 'bit))
	 (s3 (make-array nil :initial-element 0 :element-type 'bit))
	 (result (bit-ior s1 s2 s3)))
    (values s1 s2 s3 result (eqt s3 result)))
  #0a1
  #0a1
  #0a1
  #0a1
  t)

(deftest bit-ior.7
  (let* ((s1 (make-array nil :initial-element 0 :element-type 'bit))
	 (s2 (make-array nil :initial-element 1 :element-type 'bit))
	 (result (bit-ior s1 s2 t)))
    (values s1 s2 result (eqt s1 result)))
  #0a1
  #0a1
  #0a1
  t)


;;; Tests on bit vectors

(deftest bit-ior.8
  (let ((a1 (copy-seq #*0011))
	(a2 (copy-seq #*0101)))
    (values (check-values (bit-ior a1 a2)) a1 a2))
  #*0111 #*0011 #*0101)

(deftest bit-ior.9
  (let* ((a1 (copy-seq #*0011))
	 (a2 (copy-seq #*0101))
	 (result (check-values (bit-ior a1 a2 t))))
    (values result a1 a2 (eqt result a1)))
  #*0111 #*0111 #*0101 t)

(deftest bit-ior.10
  (let* ((a1 (copy-seq #*0011))
	 (a2 (copy-seq #*0101))
	 (a3 (copy-seq #*1110))
	 (result (check-values (bit-ior a1 a2 a3))))
    (values result a1 a2 a3 (eqt result a3)))
  #*0111 #*0011 #*0101 #*0111 t)

(deftest bit-ior.11
  (let ((a1 (copy-seq #*0011))
	(a2 (copy-seq #*0101)))
    (values (check-values (bit-ior a1 a2 nil)) a1 a2))
  #*0111 #*0011 #*0101)

;;; Tests on bit arrays

(deftest bit-ior.12
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))))
	 (result (bit-ior a1 a2)))
    (values a1 a2 result))
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 1)(1 1)))

(deftest bit-ior.13
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))))
	 (result (bit-ior a1 a2 t)))
    (values a1 a2 result))
  #2a((0 1)(1 1))
  #2a((0 0)(1 1))
  #2a((0 1)(1 1)))

(deftest bit-ior.14
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))))
	 (result (bit-ior a1 a2 nil)))
    (values a1 a2 result))
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 1)(1 1)))

(deftest bit-ior.15
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))))
	 (a3 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(0 0))))
	 (result (bit-ior a1 a2 a3)))
    (values a1 a2 a3 result))
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 1)(1 1))
  #2a((0 1)(1 1)))

;;; Adjustable arrays

(deftest bit-ior.16
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))
			 :adjustable t))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))
			 :adjustable t))
	 (result (bit-ior a1 a2)))
    (values a1 a2 result))
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 1)(1 1)))

;;; Displaced arrays

(deftest bit-ior.17
  (let* ((a0 (make-array '(8) :element-type 'bit
			 :initial-contents '(0 1 0 1 0 0 1 1)))
	 (a1 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 0))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 4))
	 (result (bit-ior a1 a2)))
    (values a0 a1 a2 result))
  #*01010011
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 1)(1 1)))

(deftest bit-ior.18
  (let* ((a0 (make-array '(8) :element-type 'bit
			 :initial-contents '(0 1 0 1 0 0 1 1)))
	 (a1 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 0))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 4))
	 (result (bit-ior a1 a2 t)))
    (values a0 a1 a2 result))
  #*01110011
  #2a((0 1)(1 1))
  #2a((0 0)(1 1))
  #2a((0 1)(1 1)))

(deftest bit-ior.19
  (let* ((a0 (make-array '(12) :element-type 'bit
			 :initial-contents '(0 1 0 1 0 0 1 1 1 1 1 0)))
	 (a1 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 0))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 4))
	 (a3 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 8))
	 (result (bit-ior a1 a2 a3)))
    (values a0 a1 a2 result))
  #*010100110111
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 1)(1 1)))

(deftest bit-ior.20
  (macrolet ((%m (z) z)) (bit-ior (expand-in-current-env (%m #*0011)) #*0101))
  #*0111)

(deftest bit-ior.21
  (macrolet ((%m (z) z)) (bit-ior #*1010 (expand-in-current-env (%m #*1100))))
  #*1110)

(deftest bit-ior.22
  (macrolet ((%m (z) z)) (bit-ior #*10100011 #*01101010
				  (expand-in-current-env (%m nil))))
  #*11101011)

(deftest bit-ior.order.1
  (let* ((s1 (make-array 1 :initial-element 0 :element-type 'bit))
	 (s2 (make-array 1 :initial-element 0 :element-type 'bit))
	 (x 0) y z)
    (values
     (bit-ior (progn (setf y (incf x)) s1)
	      (progn (setf z (incf x)) s2))
     x y z))
  #*0 2 1 2)

(def-fold-test bit-ior.fold.1 (bit-ior #*00101 #*10100))

;;; Random tests

(deftest bit-ior.random.1
  (bit-random-test-fn #'bit-ior #'logior)
  nil)

;;; Error tests

(deftest bit-ior.error.1
  (signals-error (bit-ior) program-error)
  t)

(deftest bit-ior.error.2
  (signals-error (bit-ior #*000) program-error)
  t)

(deftest bit-ior.error.3
  (signals-error (bit-ior #*000 #*0100 nil nil)
		 program-error)
  t)
