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


(defmodel clock ()
  ((time :initarg :clock-time
	 :accessor clock-time
	 :documentation "Current simulation time in seconds.")
   (period :cell nil
	   :initarg :clock-period
	   :type float
	   :initform 1.0
	   :accessor clock-period
	   :documentation "Period (in seconds) with which time is updated.")
   (ref-time :cell nil
             :initform (get-current-real-time-normalized)
             :type float
             :accessor clock-ref-time
             :documentation "Reference real time."))
  (:default-initargs :clock-time (c-in 0.0)))


(defun get-current-real-time-normalized ()
  (coerce (/ (get-internal-real-time) internal-time-units-per-second)
          'float))


(defun-public reset-clock (&key (clock (simulator-clock (get-simulator))))
  (setf (clock-time clock) 0)
  (setf (clock-ref-time clock) (get-current-real-time-normalized)))


(defun-public current-time (&key (clock (simulator-clock (get-simulator))))
  (clock-time clock))


(defun-public update-clock (&key (clock (simulator-clock (get-simulator))))
  (setf (clock-time clock)
        (- (get-current-real-time-normalized) (clock-ref-time clock))))


(defun-public set-clock (time &key (clock (simulator-clock (get-simulator))))
  (setf (clock-time clock) time)
  (setf (clock-ref-time clock) (- (get-current-real-time-normalized)
                                         time)))


(defun-public increase-clock-time (time &key (clock (simulator-clock (get-simulator))))
  (set-clock (+ (clock-time clock) time)))


(defmethod print-object ((self clock) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "time: ~a, period: ~a" (clock-time self)
	    (clock-period self))))
