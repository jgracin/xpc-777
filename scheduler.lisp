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


;;; This package enables scheduling of actions which are expected
;;; to be performed at some later simulation time.
;;; Scheduled actions can be tagged so that one can find them
;;; by tag, delete them by tag, etc.

(defclass scheduler-action ()
  ((timestamp :initarg :timestamp
              :accessor action-timestamp)
   (tag :initarg :tag
        :initform nil
        :accessor action-tag)
   (fn :initarg :fn
       :accessor action-fn)))


(defun make-scheduler-action (timestamp fn &optional (tag nil))
  (make-instance 'scheduler-action :timestamp timestamp :fn fn :tag tag))


(defmethod print-object ((self scheduler-action) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream "at ~a, tag ~s, call ~s"
            (action-timestamp self)
            (action-tag self)
            (third (multiple-value-list         
                       (function-lambda-expression (action-fn self)))))))


(defmodel scheduler (family)
  ((timer :initform (c? (scheduler-fn self))
          :accessor scheduler-timer)
   (timetable :cell nil
              :initform (make-container
                         'priority-queue-on-container
                         :sorter (lambda (a1 a2)
                                   (< (action-timestamp a1)
                                      (action-timestamp a2)))
                         :test
                         (lambda (a1 a2) (and (eql (action-timestamp a1)
                                                   (action-timestamp a2))
                                              (eql (action-tag a1)
                                                   (action-tag a2)))))
              :accessor scheduler-timetable)))


(defun scheduler-fn (scheduler)
  (let ((timetable (scheduler-timetable scheduler))
        (now (current-time)))
    (when (plusp (size timetable))
      (loop until (empty-p timetable)
        for action = (first-element timetable)
        until (> (action-timestamp action) now)
        do
        (delete-first timetable)
        (funcall (action-fn action))))))


(defparameter *scheduler* (make-instance 'scheduler))


(defun-public find-actions-by-tag (tag &key (scheduler *scheduler*))
  (let ((result nil))
    (iterate-elements (scheduler-timetable scheduler)
                      (lambda (e)
                        (when (eql (action-tag e) tag)
                          (push e result))))
    result))


(defun-public delete-actions-by-tag (tag &key (scheduler *scheduler*))
  (loop for action in (find-actions-by-tag tag :scheduler scheduler)
        do (delete-element (scheduler-timetable scheduler) action)))


(defun-public delete-all-actions (&key (scheduler *scheduler*))
  (empty! (scheduler-timetable scheduler)))


(defun-public schedule (at-time function &key (scheduler *scheduler*) tag (relative-time-p t))
  "Schedules an action which will be called at specified time.  Function
will be called without arguments."
  (let ((time (if relative-time-p
                  (+ at-time (current-time))
                  at-time)))
    (insert-item (scheduler-timetable scheduler)
                 (make-scheduler-action time function tag))))


(defun print-scheduler-queue (&optional (scheduler *scheduler*))
  (iterate-elements (scheduler-timetable scheduler) #'print))


(defun print-debug-and-reschedule ()
  (logdebug "Scheduler tick, time is ~a" (current-time))
  (schedule 5.0 #'print-debug-and-reschedule))


(define-init-function initialize-scheduler ()
  (logdebug "Initializing scheduler")
  (delete-all-actions :scheduler *scheduler*)
  (schedule 0.0 #'print-debug-and-reschedule))



