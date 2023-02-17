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


(defmodel light-switches (family)
  ())


(defmodel lights (system)
  ())


(defmodel light (component)
  ((status :initarg :status
	   :accessor light-status)))


(defun b777-pass-signs-light-logic (pass-signs-light)
  (declare (ignore pass-signs-light))
  (let ((switch-position (switch-position (sys :pass-signs-switch)))
	(altitude (aircraft-altitude (sys :flight-parameters))))
    (if (not (eql switch-position :auto))
	switch-position
	(cond
	  ((not (eql (lever-position (sys :gear-lever)) :up)) :on)
	  ((> (lever-position (sys :flaps-lever)) 0) :on)
	  ((< altitude 10300) :on)
	  ((and (>= altitude 18000) (< altitude 39000)) :off)
	  (t :on)))))


(defun b777-no-smoking-signs-light-logic (no-smoking-signs-light)
  (declare (ignore no-smoking-signs-light))
  (logdebug "Calling b777-no-smoking-signs-light-logic")
  (if (eql :off (switch-position (sys :no-smoking-signs-switch)))
      :off
      :on))


;;; Utility

(defun set-pass-signs (pos)
  (setf (switch-position (sys :pass-signs-switch)) pos))


(defun set-no-smoking-signs (pos)
  (setf (switch-position (sys :no-smoking-signs-switch)) pos))
