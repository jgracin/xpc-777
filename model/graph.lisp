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
;;; Functions for traversal of graph.
;;;
;;; Works on instances of graph-component-mixin.
;;;


(defmodel graph-component-mixin ()
  ((neighs :initarg :neighs
           :accessor graph-component-neighs
           :documentation "Systems directly connected to this system."))
  (:default-initargs
      :neighs (c? nil)))




