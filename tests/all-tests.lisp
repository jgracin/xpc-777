(in-package :xpc)

(def-suite xpc-suite :description "Tests for xpc package.")


(defun set-clean-configuration ()
  (set-flaps 0)
  (set-gear :up))
