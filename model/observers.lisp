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

;;;
;;; Switches
;;;
(defobserver switch-position ((self switch) new-value old-value)
  (logdebug "switch position change, ~a, old=~a, new=~a, handler=~a"
            self old-value new-value (switch-change-handler self))
  (if (typep self 'elec-switch)
      (ecu-handle-switch-change self new-value old-value))
  (when (switch-change-handler self)
    (funcall (switch-change-handler self) self new-value old-value)))


(defobserver state ((self elec-relay) new-value old-value)
  (logdebug "elec relay state change, ~a, old=~a, new=~a, handler=~a"
            self old-value new-value (pdn-relay-change-handler self))
  (when (pdn-relay-change-handler self)
    (funcall (pdn-relay-change-handler self) self new-value old-value))  )


(defobserver available-p ((self elec-source) new-value old-value)
  (logdebug "change of source availability status for ~a, from ~a to ~a"
            self old-value new-value)
  (ecu-handle-source-available-change self new-value old-value))
