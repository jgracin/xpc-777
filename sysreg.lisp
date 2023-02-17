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

;; System registry
(defparameter *systems-cache* (make-hash-table)
  "Used for caching results of queries for aircraft's systems.")


(define-init-function initialize-systems-cache ()
  (setq *systems-cache* (make-hash-table)))


(defun-public sys (name &optional (caching-p nil))
  "Returns an aircraft system by name.  If name is already resolved to a
system object, that object is returned without further resolving."
  (if (not (keywordp name))
      name
      (if (not caching-p)
          (fm-find-one (current-aircraft) name :must-find nil)
          (let ((existing (gethash name *systems-cache*)))
            (if existing
                existing
                (let ((system (fm-find-one (current-aircraft) name :must-find nil)))
                  (unless system
                    (error "Could not find system ~a!" name))
                  (setf (gethash name *systems-cache*) system)
                  system))))))


(defun-public system-name (sys)
  "Returns the name of the system.  This is the name with which the system
is registered and can be obtained by using function SYS.

For convenience, if called with keyword as an argument, it assumes that
already is the name of the system and returns that same keyword."
  (if (keywordp sys)
      sys
      (md-name sys)))
