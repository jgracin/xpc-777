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

(defparameter *mouse-left-button* 1)

(defparameter *mouse-middle-button* 2)

(defparameter *mouse-right-button* 3)


(defun image-file (name)
  (let ((filename (string-downcase (etypecase name
                                     (symbol (symbol-name name))
                                     (string name)))))
    (namestring
     (merge-pathnames
      (make-pathname :directory
                     '(:absolute "home" "gracin" "src" "lisp" "xpc" "images"))
      (make-pathname :name filename :type "png")))))


(defgeneric handle-mouse-click (component x y button)
  (:documentation "Called when user clicks somewhere on a component."))


(defgeneric display-component (panel-component)
  (:documentation "Redraws a panel component."))


(defun display-panel (panel)
  (sdl:window (panel-width panel) (panel-height panel))
  (sdl:blit-surface (panel-surface panel))
  (update-panel panel))


(defun update-panel (panel)
  (loop for component in (kids panel)
        do (display-component component)))


