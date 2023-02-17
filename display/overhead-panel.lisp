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


(defun overhead-panel-scheme ()
  (list
   (list *b777-adiru-panel*
         *b777-emer-lights-panel*
         *b777-start-panel*
         *b777-air-cond-panel*)
   (list *b777-electrical-panel*
         *b777-hydraulic-panel*
         *b777-fuel-panel*
         *b777-bleed-air-panel*)
   (list *b777-left-wiper-panel*
         *b777-pass-signs-panel*
         *b777-anti-ice-panel*
         *b777-right-wiper-panel*)))


(defun overhead-subpanel-at (row column)
  "Returns subpanel \(i.e. some of the panels from overhead panel scheme\)
with coordinates \(row,col\).  Both row and col start from 0."
  (when (and (>= row 0)
             (<= row 2)
             (>= column 0)
             (<= column 3))
    (elt (elt (overhead-panel-scheme) row) column)))


(defun find-panel (panel)
  (loop for row in (overhead-panel-scheme)
        for row-number from 0
        for column-number = (position panel row)
        when column-number return (list row-number column-number)))


(defun get-neigh-panel (panel direction)
  "Returns a panel which is left, right, up, or down with respect to the
given panel.  Direction can be one of :UP, :DOWN, :LEFT, or :RIGHT."
  (let* ((coords (find-panel panel))
         row
         column)
    (unless coords
      (error "Invalid panel ~a" panel))
    (setq row (first coords))
    (setq column (second coords))
    (case direction
      (:up (decf row))
      (:down (incf row))
      (:left (decf column))
      (:right (incf column)))
    (let ((result (overhead-subpanel-at row column)))
      (if result
          result
          panel))))
