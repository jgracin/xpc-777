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



(defmodel panel (family)
  ((surface :cell nil
            :initarg :surface
            :accessor panel-surface)
   (width :cell nil
          :initarg :width
          :accessor panel-width)
   (height :cell nil
           :initarg :height
           :accessor panel-height))
  (:default-initargs
      :kids (c-in nil)))


(defmethod handle-mouse-click ((panel panel) x y button)
  "Handles a mouse click on panel, coordinates (x, y) with given button."
  (let ((margin 20))
    (cond
      ((< x margin) (get-neigh-panel panel :left))
      ((> x (- (panel-width panel) margin)) (get-neigh-panel panel :right))
      ((< y margin) (get-neigh-panel panel :up))
      ((> y (- (panel-height panel) margin)) (get-neigh-panel panel :down))
      (t (progn
           (loop for component in (kids panel)
                 do (handle-mouse-click component x y button))
           panel)))))
