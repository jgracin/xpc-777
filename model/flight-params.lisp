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


(defmodel flight-parameters (family)
  ((speed :cell t
          :initarg :speed
          :accessor aircraft-speed)
   (altitude :cell t
             :initarg :altitude
             :accessor aircraft-altitude)
   (heading :cell t
            :initarg :heading
            :accessor aircraft-heading)
   (autoland-in-progress-p :cell t
                           :initarg :autoland-in-progress-p
                           :accessor aircraft-autoland-in-progress-p))
  (:default-initargs
      :speed (c-in 0)
    :altitude (c-in 0)
    :heading (c-in 0)
    :autoland-in-progress-p (c-in nil)))


;;; Utility functions

(defun-public set-altitude (altitude)
  (setf (aircraft-altitude (sys :flight-parameters)) altitude))
