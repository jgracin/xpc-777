(in-package :cl-user)

(defpackage :xpc
    (:use #:cl #:it.bese.fiveam #:cl-containers #:sb-bsd-sockets #:ieee-floats #:cells)
  (:import-from :jgutil
                #:defmacro-public
                #:defun-public
                #:defgeneric-public)
  (:export
   #:switch-led-status
   #:switch-position
   #:pdn-relay-state))

