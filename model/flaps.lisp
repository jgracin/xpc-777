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



(defmodel flaps-lever (lever)
  ())


(defmodel b777-flaps-lever (flaps-lever)
  ())


(defgeneric allowed-flaps-lever-position-p (flaps-lever pos)
  (:documentation "Returns true if 'pos' represents an allowed setting for
flaps lever."))


(defmethod allowed-flaps-lever-position-p ((lever b777-flaps-lever) pos)
  (let ((allowed-position-values '(0 1 5 15 20 25 30)))
    (values (member pos allowed-position-values)
	    allowed-position-values)))


(defmodel flaps-surface (component)
  ((pos :initarg :flaps-surface-position
        :accessor flaps-surface-position)
   (last-pos :cell nil
	     :initarg :flaps-surface-last-pos
             :accessor flaps-surface-last-pos))
  (:default-initargs
      :flaps-surface-position (c_? (flaps-surface-position-calculator self))
    :flaps-surface-last-pos 0)) 


(defmodel b777-flaps-surface (flaps-surface) ())


(defgeneric flaps-surface-position-calculator (flaps-surface))


(defmethod flaps-surface-position-calculator ((self flaps-surface))
  (declare (ignore self))
  (lever-position (sys :flaps-lever)))


(defvar *flaps-transit-time-resolution* 1.0
  "Time resolution with which new flaps position is calculated when in
transit.")


(defmethod flaps-surface-position-calculator ((self b777-flaps-surface))  
  (flet ((calculate-full-transition-time (start-position end-position)
	   (abs (- start-position end-position))))
    (let* ((current-position (flaps-surface-last-pos self))
	   (lever-position (lever-position (sys :flaps-lever)))
	   (transition-end-time (+ (current-time)
				   (calculate-full-transition-time current-position
								   lever-position))))
      (when (not (eql lever-position current-position))
	(schedule *flaps-transit-time-resolution*
		  (lambda ()
		    (setf (flaps-surface-last-pos self)
			  (cond ((> (current-time) transition-end-time)
				 lever-position) 
				((> (lever-position (sys :flaps-lever))
				    (flaps-surface-last-pos self))
				 (min (lever-position (sys :flaps-lever))
				      (1+ (flaps-surface-last-pos self))))
				((< (lever-position (sys :flaps-lever))
				    (flaps-surface-last-pos self))
				 (max (lever-position (sys :flaps-lever))
				      (1- (flaps-surface-last-pos self))))
				(t (lever-position (sys :flaps-lever))))))
		  :tag :flaps-surface-position-calculator))))
  (flaps-surface-last-pos self))


(defmodel b777-flaps-surface (flaps-surface)
  ()
  (:default-initargs
      :flaps-surface-position (c_? (flaps-surface-position-calculator self))))


(defmodel flaps-system (system) ())


(defmodel b777-flaps-system (flaps-system) ())


(defmodel flaps-indicator (indicator)
  ()
  (:default-initargs
      :value (c_? (flaps-indicator-logic self))))


(defgeneric flaps-indicator-logic (flaps-indicator)
  (:documentation "Returns the value of flaps indicator."))


(defmethod flaps-indicator-logic (flaps-indicator)
  (declare (ignore flaps-indicator))
  (flaps-surface-position (sys :left-inboard-flap)))


(defobserver value (self)
  (logdebug "~s value: ~a" (type-of self) (indicator-value self)))


(define-init-function initialize-flaps ()
  (setf (lever-position (sys :flaps-lever)) 0)
  (delete-actions-by-tag :flaps-surface-position-calculator)
  (indicator-value (sys :flaps-indicator)))


;;; Utilities

(defun set-flaps (flaps-setting &key (flaps-lever (sys :flaps-lever)))
  (unless (allowed-flaps-lever-position-p flaps-lever flaps-setting)
    (error "Illegal flaps lever position: ~a" flaps-setting))
  (setf (lever-position (sys :flaps-lever)) flaps-setting))


(defun current-flaps-position ()
  (indicator-value (sys :flaps-indicator)))
