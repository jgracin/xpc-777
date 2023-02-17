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

(defmodel b777-electrical-panel (panel)
  ())


(defparameter *b777-electrical-panel*
  (xpc:make-system b777-electrical-panel :electrical-panel
                   :surface (image-surface :electrical-panel)
                   :width 477 :height 596
                   :subsystems
                   (on/off-pushbutton :main-battery-switch
                                      :x 210 :y 63
                                      :model-object (xpc:sys :main-battery-switch)
                                      :led-status (c_? (xpc:switch-led-status :main-battery-switch)))
                   (on/off-pushbutton :ife-switch
                                      :x 56 :y 118
                                      :model-object (xpc:sys :ife-switch)
                                      :led-status (c_? (xpc:switch-led-status :ife-switch)))
                   (on/off-pushbutton :utility-switch
                                      :x 128 :y 118
                                      :model-object (xpc:sys :utility-switch)
                                      :led-status (c_? (xpc:switch-led-status :utility-switch)))
                   (on/off-pushbutton :apu-gen-switch
                                      :x 208 :y 167
                                      :model-object (xpc:sys :apu-gen-switch)
                                      :led-status (c_? (xpc:switch-led-status :apu-gen-switch))) 
                   (apu-start-switch :apu-start-switch
                                     :x 352 :y 71
                                     :model-object (xpc:sys :apu-start-switch))
                   (on/off-pushbutton :l-gen-ctrl-switch
                                      :x 55 :y 432
                                      :model-object (xpc:sys :l-gen-ctrl-switch)
                                      :led-status (c_? (xpc:switch-led-status :l-gen-ctrl-switch)))
                   (on/off-pushbutton :r-gen-ctrl-switch
                                      :x 363 :y 432
                                      :model-object (xpc:sys :r-gen-ctrl-switch)
                                      :led-status (c_? (xpc:switch-led-status :r-gen-ctrl-switch)))
                   (on/off-pushbutton :l-bu-gen-switch
                                      :x 177 :y 464
                                      :model-object (xpc:sys :l-bu-gen-switch) 
                                      :led-status (c_? (xpc:switch-led-status :l-bu-gen-switch)))
                   (on/off-pushbutton :r-bu-gen-switch
                                      :x 242 :y 464
                                      :model-object (xpc:sys :r-bu-gen-switch)
                                      :led-status (c_? (xpc:switch-led-status :r-bu-gen-switch)))
                   (auto/isln-pushbutton :l-bus-tie-switch
                                         :x 55 :y 231
                                         :model-object (xpc:sys :l-bus-tie-switch)
                                         :led-status (c_?
                                                       (xpc:switch-led-status :l-bus-tie-switch)))
                   (auto/isln-pushbutton :r-bus-tie-switch
                                         :x 363 :y 231
                                         :model-object (xpc:sys :r-bus-tie-switch)
                                         :led-status (c_?
                                                       (xpc:switch-led-status :r-bus-tie-switch)))
                   (on/avail-pushbutton :pri-ext-pwr-switch
                                        :x 274 :y 307
                                        :model-object (xpc:sys :pri-ext-pwr-switch)
                                        :led-status (c_? (xpc:switch-led-status :pri-ext-pwr-switch)))
                   (on/avail-pushbutton :sec-ext-pwr-switch
                                        :x 144 :y 307
                                        :model-object (xpc:sys :sec-ext-pwr-switch) 
                                        :led-status (c_? (xpc:switch-led-status :sec-ext-pwr-switch)))))



