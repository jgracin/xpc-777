;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Copyright (C) 2007 Josip Gracin.
;;;
;;; This file is part of XPC-777.
;;;
;;; XPC-777 is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;

(in-package :xplane)


(defun parse-type (msg-buffer)
  #+sbcl
  (sb-ext:octets-to-string msg-buffer :end 4)
  #-sbcl
  (when (>= (length msg-buffer) 4)    
    (concatenate 'string
		 (string (code-char (aref msg-buffer 0)))
		 (string (code-char (aref msg-buffer 1)))
		 (string (code-char (aref msg-buffer 2)))
		 (string (code-char (aref msg-buffer 3))))))

(defun parse-internal-index-byte (msg-buffer)
  "Check UDP docs for X-Plane."
  (elt msg-buffer 4))


(defun parse-data-msg-values (msg-buffer)
  "Returns a list of pairs containing parameters and their values.  Each
pair is a pair of parameter id and a list of 8 values corresponding to
that parameter."  
  (let ((index-of-first-block 5)
	(block-size 36))
    (loop for block-index
      from index-of-first-block
      upto (1- (length msg-buffer)) by block-size
      collect
      (let ((parameter-id (parse-xint (subseq msg-buffer block-index (+ 4 block-index))))
	    (values (loop for cnt from (+ 4 block-index)
			  upto (1- (+ block-size block-index)) by 4
			  collect (parse-xflt (subseq msg-buffer cnt (+ 4 cnt))))))
	(list parameter-id values)))))


(defun parse-data-msg-parameter-id (msg-buffer)
  "Returns a parameter id value in DATA message.  Check UDP docs for
X-Plane."
  (parse-xint (subseq msg-buffer 5 9)))


(defun parse-xchr (msg-buffer)
  (aref msg-buffer 0))


(defun parse-xint (msg-buffer)
  (let ((result 0))
    (setf (ldb (byte 8 24) result) (aref msg-buffer 0))
    (setf (ldb (byte 8 16) result) (aref msg-buffer 1))
    (setf (ldb (byte 8 8) result) (aref msg-buffer 2))
    (setf (ldb (byte 8 0) result) (aref msg-buffer 3))
    result))


(defun parse-xflt (msg-buffer)
  (decode-float32 (parse-xint msg-buffer)))


(defun write-xbyte (byte buffer)
  (vector-push byte buffer))

(defun write-xchr (char buffer)
  (vector-push (char-code char) buffer))

(defun write-xchrs (string buffer)
  "Writes a sequence of characters to buffer.  Use instead of calling
write-xchr multiple times."
  (loop for c across string
	do (write-xchr c buffer)))

(defun write-xint (int buffer)
  (vector-push (ldb (byte 8 24) int) buffer)
  (vector-push (ldb (byte 8 16) int) buffer)
  (vector-push (ldb (byte 8 8) int) buffer)
  (vector-push (ldb (byte 8 0) int) buffer)
  buffer)

(defun write-xflt (float buffer)
  (let ((f (encode-float32 float)))
    (vector-push (ldb (byte 8 24) f) buffer)
    (vector-push (ldb (byte 8 16) f) buffer)
    (vector-push (ldb (byte 8 8) f) buffer)
    (vector-push (ldb (byte 8 0) f) buffer)
    buffer))

(defun write-xdob (double buffer)
  (let ((f (encode-float64 double)))
    (vector-push (ldb (byte 8 56) f) buffer)
    (vector-push (ldb (byte 8 48) f) buffer)
    (vector-push (ldb (byte 8 40) f) buffer)
    (vector-push (ldb (byte 8 32) f) buffer)
    (vector-push (ldb (byte 8 24) f) buffer)
    (vector-push (ldb (byte 8 16) f) buffer)
    (vector-push (ldb (byte 8 8) f) buffer)
    (vector-push (ldb (byte 8 0) f) buffer)
    buffer))

(defun write-prologue (type-of-message buffer &key (internal-index-byte 0))
  (let* ((error-msg "Type of message must be a four letter string or symbol and it was ~a!")
	 (type (cond ((symbolp type-of-message) (symbol-name type-of-message))
		     ((stringp type-of-message) type-of-message)
		     (t (error error-msg type-of-message)))))
    (unless (= 4 (length type))
      (error error-msg type))
    (write-xchrs type buffer)
    (write-xbyte internal-index-byte buffer)))


(defun finalize-data-buffer (buffer)
  "Adds padding to buffer which contains DATA."
  (adjust-array buffer (array-dimension buffer 0) :fill-pointer 41))



