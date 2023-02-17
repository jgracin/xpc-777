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


(defparameter *image-surface-cache* (make-hash-table))


(xpc:define-init-function initialize-image-surface-cache ()
  (setq *image-surface-cache* (make-hash-table)))


(defun image-surface (name)
  "Returns SDL surface which is loaded from image file with given name.
The result is memoized, so that images get loaded only once."
  (let ((cached-value (gethash name *image-surface-cache*)))
    (or cached-value
        (let ((surface (sdl-image:load-image (image-file name))))
          (setf (gethash name *image-surface-cache*) surface)
          surface))))
