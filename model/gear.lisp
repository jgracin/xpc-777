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


(defmodel gear-lever (lever) ())


(defmodel gear-system (system) ()
	  (:documentation "Gear system might contain gears, gear lever,
gear indicator etc."))


(defmodel b777-gear-system (system) ())


(defmodel gear (component)
  ((pos :initarg :gear-position
	:accessor gear-position))
  (:default-initargs 
      :gear-position (c_? (gear-position-calculator self))))


(defgeneric gear-position-calculator (gear)
  (:documentation "Returns current real position of landing gear.  Real means
where the gear currently really are, not at what position is the lever."))


(defmodel b777-gear (gear) ())


(defmethod gear-position-calculator ((self b777-gear))
  (lever-position (sys :gear-lever)))


(defmodel gear-indicator (indicator)
 ()
 (:default-initargs
     :value (c_? (gear-indicator-logic self))))


(defgeneric gear-indicator-logic (indicator))


(defmethod gear-indicator-logic ((self gear-indicator))
  (gear-position (sys :left-main-gear)))


;;; Utility

(defun current-gear-position ()
  (indicator-value (sys :gear-indicator)))


(defun set-gear (gear-setting)
  (setf (lever-position (sys :gear-lever)) gear-setting))
