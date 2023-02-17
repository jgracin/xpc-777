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


(defun main-loop (&key (reinit t))
  (when reinit
    (xpc:initialize-simulator))
  (sdl:with-init ()
    (sdl:set-frame-rate 5)
    (let ((panel *b777-electrical-panel*))
      (display-panel panel)
      (sdl:with-events ()
        (:quit-event () t)
        (:mouse-button-down-event (:button button :state state :x x :y y)
                                  (logdebug "Mouse ~a, ~a pos x=~a, y=~a" button state x y) 
                                  (let ((next-panel (handle-mouse-click panel x y button)))
                                    (when (not (eql next-panel panel))
                                      (setq panel next-panel)
                                      (display-panel panel)))
                                  (sdl:update-display))
        (:idle () (progn
                    (xpc:update-clock)
                    (update-panel panel)
                    (sdl:update-display)))
        (:video-expose-event (sdl:update-display))))))


(defun start ()
  (sb-thread:make-thread #'main-loop))
