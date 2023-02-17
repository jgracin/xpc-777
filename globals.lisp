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


;;;
;;; Logging
;;;

(defvar *log* (make-instance 'log4cl:logger))

(defvar *console-appender* (make-instance 'log4cl:console-appender))

(unless (log4cl:get-appender *log* 'console-appender)
  (log4cl:add-appender *log* *console-appender*))


(defun-public set-log-level (level)
  (log4cl:set-level *log* level))


(set-log-level :debug)


(defmacro-public logdebug (&rest args)
  `(log4cl:log-debug *log* (format nil ,@args)))


(defmacro-public loginfo (&rest args)
  `(log4cl:log-info *log* (format nil ,@args)))


(defmacro-public logerror (&rest args)
  `(log4cl:log-error *log* (format nil ,@args)))


;;;
;;; Simulator stuff
;;;
;; Needs to be defined now, but is set later when aircraft is constructed
(defvar *default-aircraft*)

(defun-public current-aircraft ()
  *default-aircraft*)

;; Set in simulator.lisp
(defvar *simulator*)

(defun-public get-simulator ()
  *simulator*)

