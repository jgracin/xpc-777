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


(defmodel component (family) ())


(defmodel system (family) ()
	  (:documentation "An aircraft system which might consist of
several components."))


(defmodel switch (component)
  ((switch-position :cell t
                    :initarg :switch-position
                    :accessor switch-position
                    :documentation "Represents a switch.")
   (change-handler :cell nil
                   :initform nil
                   :initarg :change-handler
                   :reader switch-change-handler
                   :documentation "Function which will get called when
switch changes position.")
)
  (:default-initargs
      :switch-position (c-in :off)))


(defmodel indicator (component)
  ((value :initarg :value
	  :accessor indicator-value)))


(defmodel lever (component)
  ((lever-position :initargs :lever-position
                   :initform (c-in 0)
                   :accessor lever-position))
  (:documentation "Base class for all levers within cockpit."))
