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


(defmodel b777-hydraulic-panel (panel)
  ())


(defparameter *b777-hydraulic-panel*
  (xpc:make-system b777-hydraulic-panel :hydraulic-panel
                   :surface (sdl-image:load-image (image-file :hydraulic-panel))
                   :width 477
                   :height 596
                   #+nil :kids
                   #+nil (list
                          (xpc:mksys 'off/auto/on-switch :l-elec-demand-switch
                                     :x 42 :y 426)
                          (xpc:mksys 'off/auto/on-switch :c1-air-switch
                                     :x 147 :y 410)
                          (xpc:mksys 'off/auto/on-switch :c2-air-switch
                                     :x 255 :y 410)
                          (xpc:mksys 'off/auto/on-switch :r-elec-demand-switch
                                     :x 361 :y 426))))
