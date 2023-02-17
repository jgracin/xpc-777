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


(defvar *init-functions* nil
  "A list of function names representing functions which will be called
each time the simulator starts.")


(defmacro-public define-init-function (fn-name &rest args)
  "Adds a function to be called whenever the simulation starts.  Just use
define-init-function instead of defun for functions which you want to be
run when simulation begins."
  `(progn
    (pushnew ',fn-name *init-functions*)
    (defun ,fn-name ,@args)))


(defun call-init-functions ()
  (loop for fn in *init-functions*
	do (funcall fn)))



(defmacro %make-system (parent class-name system-name &rest args)
  (let ((new-instance (gensym "NEW-INSTANCE-")))
    (let ((subsys-keyword-index (position :subsystems args))
          subsystems
          params)
      (when subsys-keyword-index
        (setf subsystems (subseq args (1+ subsys-keyword-index)))
        (if (> subsys-keyword-index 0)
            (setf params (subseq args 0 subsys-keyword-index))))
      (if (and (null subsys-keyword-index)
               args)
          (setf params args))
      `(let ((,new-instance (make-instance ',class-name :md-name ,system-name
                                           ,@(when parent `(:fm-parent ,parent))
                                           ,@(when params params))))
        ,@(loop for subsys in subsystems
                collect `(push (%make-system ,new-instance ,@subsys)
                          (kids ,new-instance)))
        ,new-instance))))


(defmacro-public make-system (class-name system-name &rest args)
  `(%make-system nil ,class-name ,system-name ,@args))


(defun-public toggle-on-off (value)
  "Returns opposite of :off and :on."
  (case value
    (:off :on)
    (:on :off)))


(defun-public the-other-value (value values)
  "Values is a two element list.  Value is one of those elements.  The
returned value is the other element."
  (if (eql value (first values))
      (second values)
      (first values)))

