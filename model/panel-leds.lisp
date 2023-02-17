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
;;; Logic for various LEDs on panels
;;;

(defun breaker-to-led-status (status)
  "Converts :open and :closed to :on and :off, respectively."
  (case status
    (:open :on)
    (:closed :off)))


(defun-public switch-led-status (switch)
  (case (system-name switch)
    ((:l-bus-tie-switch :r-bus-tie-switch) (case (switch-position (sys switch))
                                             (:auto :off)
                                             (:off :isln)))
    (:l-gen-ctrl-switch
     (breaker-to-led-status (pdn-relay-state (sys :l-gcb))))
    (:r-gen-ctrl-switch
     (breaker-to-led-status (pdn-relay-state (sys :r-gcb))))
    (otherwise (xpc:toggle-on-off (xpc:switch-position (xpc:sys switch))))))

