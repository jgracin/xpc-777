;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :cl-user)

(defpackage :xplane
    (:use :cl #:it.bese.fiveam #:cl-containers #:sb-bsd-sockets #:ieee-floats #:cells #:xpc)
  (:import-from :jgutil
                #:defmacro-public
                #:defun-public
                #:defgeneric-public))
