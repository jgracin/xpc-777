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

(in-package :xpc.disp)


(defmodel b777-pass-signs-panel (panel)
  ())


(defparameter *b777-pass-signs-panel*
  (xpc:make-system b777-pass-signs-panel :pass-signs-panel
                   :surface (image-surface :pass-signs-panel)
                   :width 478 :height 544
                   :subsystems
                   (off/auto/on-switch :pass-signs-switch
                              :x 218 :y 99
                              :model-object (xpc:sys :pass-signs-switch))
                   (off/auto/on-switch :no-smoking-signs-switch
                              :x 70 :y 99
                              :model-object (xpc:sys :no-smoking-signs-switch))))





