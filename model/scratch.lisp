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


(reset-elec-system)
(print-elec-system-status)

(set-engine-running-idle :apu-eng-params)
(set-engine-running-idle :l-eng-params)
(set-engine-running-idle :r-eng-params)

(setf (switch-position (sys :apu-gen-switch)) :on)
(setf (switch-position (sys :pri-ext-pwr-switch)) :on)
(setf (switch-position (sys :sec-ext-pwr-switch)) :on)
(setf (switch-position (sys :l-gen-ctrl-switch)) :on)
(setf (switch-position (sys :r-gen-ctrl-switch)) :on)

(setf (switch-position (sys :l-bus-tie-switch)) :auto)
(setf (switch-position (sys :l-bus-tie-switch)) :off)
(setf (switch-position (sys :r-bus-tie-switch)) :auto)
(setf (switch-position (sys :r-bus-tie-switch)) :off)


(defun print-elec-status-and-reschedule ()
  (print-elec-system-status)
  (schedule 2.0 #'print-elec-status-and-reschedule))


;;
(print-elec-system-status)
(set-engine-running-idle :r-eng-params)
(setf (switch-position (sys :pri-ext-pwr-switch)) :on)
(setf (switch-position (sys :r-gen-ctrl-switch)) :on)
(print-elec-system-status)
(shutdown-engine (sys :r-eng-params))
