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

(in-package :xpc)


(defmodel engine-parameters (component)
  ((n1 :initarg :n1
       :accessor engine-n1)
   (n2 :initarg :n2
       :accessor engine-n2)
   (egt :initarg :egt
       :accessor engine-egt))
  (:default-initargs
      :n1 (c-in 0) :n2 (c-in 0) :egt (c-in 20)))


;;;
;;; Debugging utils
;;;


(defun print-engine-params ()
  (let* ((systems (kids (sys :aircraft)))
         (engine-params (remove-if-not (lambda (s) (typep s 'engine-parameters))
                                       systems))) 
    (loginfo
     (with-output-to-string
       (str)
       (format str "============ Engine parameters: ===============~%")
       (loop for ep in engine-params 
             do (format str "~25<~a~>:  N1=~5@a%     N2=~5@a%     EGT=~3@adegC~%"
                        ep (engine-n1 ep) (engine-n2 ep) (engine-egt ep))) 
       (format str "~&--------------------------------------------------------------------")))
    (loginfo "=============End of engine parameters =========")
    (values)))


(defun set-engine-parameters (engine-params n1 n2 egt)
  (let ((system (sys engine-params)))
    (setf (engine-n1 system) n1)
    (setf (engine-n2 system) n2)
    (setf (engine-egt system) egt)
    (values (engine-n1 system) (engine-n2 system) (engine-egt system) system))  )

(defun set-engine-running-idle (engine-params)
  "Sets engine parameters to values of an engine running at idle thrust."
  (set-engine-parameters engine-params 20 64 350))


(defun shutdown-engine (engine-params)
  (set-engine-parameters engine-params 0 0 20))
