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
;;; Generic power distribution network
;;;
;;; Hopefully used in electrical system and hydraulic system.
;;;

(defmodel pdn-component (system graph-component-mixin)
  ())


(defmodel pdn-source (pdn-component)
  ())


(defmodel pdn-bus (pdn-component)
  ())


(defmodel pdn-sink (pdn-component)
  ())


(defmodel pdn-relay (pdn-component)
  ((pin1 :cell nil
         :initarg :pin1
         :accessor pdn-relay-pin1
         :documentation "The name of the system to which one end of the
relay is connected.  Resolve this name to object by using function sys.")
   (pin2 :cell nil
         :initarg :pin2
         :accessor pdn-relay-pin2
         :documentation "The name of the system to which one end of the
relay is connected.  Resolve this name to object by using function sys.")
   (state :cell t
          :initarg :state
          :accessor pdn-relay-state
          :documentation "Relay between two components, which can be in
:closed or in :open state.")
   (change-handler :cell nil
                   :initform nil
                   :initarg :change-handler
                   :reader pdn-relay-change-handler
                   :documentation "Function which will get called when
relay changes state."))
  (:default-initargs
      :state (c? :open)))


(defmodel pdn-relay-w/base (pdn-relay)
  ((base-pin :cell nil
                        :initarg :base-pin
                        :accessor pdn-relay-w/base-pin)))


(defmodel pdn-converter (pdn-component)
  ())


(defun pdn-find-systems (start-system &key (test #'identity) (key #'identity) (dont-cross nil))
  "Traverses a graph and returns a list of reachable nodes for which test
function returned non-nil.

Argument dont-cross represents a list of systems which MUST NOT BE traversed."
  (let ((visited-nodes-map (make-hash-table))
        result)
    (labels ((is-visited-p (node)
               (gethash (system-name node) visited-nodes-map))
             (set-visited (node)
               (setf (gethash node visited-nodes-map) t))
             (find-nodes (start-system)
               (when (funcall test (funcall key start-system))
                 (push (funcall key start-system) result))
               (set-visited (system-name start-system))
               (loop for node in (mapcar #'sys (graph-component-neighs start-system))
                     unless (is-visited-p node) 
                     do (when (not (member (system-name node) dont-cross :key #'system-name))
                          (find-nodes node)))))
      (find-nodes start-system)
      result)))


(defun find-pdn-sources (start-system &key (dont-cross nil))
  (pdn-find-systems start-system
                    :test (lambda (n)
                            (typep n 'pdn-source))
                    :dont-cross dont-cross))



;;;
;;; Neighboring between components.
;;;


(defgeneric find-connected-neighbor (this-system neigh-system)
  (:documentation "System given with argument this-system is a system
which potentially has several neighboring systems, some of which are
connected via a relay.

The logic is the following.  If neigh-system is a relay, then this
function returns the system which is behind that relay if relay is closed,
or nil if relay is opened.  If neigh-system is not a relay but some other
eletrical component, that component is returned unconditionally."))


(defmethod find-connected-neighbor ((this-system pdn-component)
                                    (neigh-system pdn-component))
  neigh-system)


(defmethod find-connected-neighbor ((this-system pdn-component) (neigh-system pdn-relay))
  (if (eql :open (pdn-relay-state neigh-system))
      nil
      (cond
        ((eql (system-name this-system)
              (pdn-relay-pin1 neigh-system))
         (sys (pdn-relay-pin2 neigh-system)))
        ((eql (system-name this-system)
              (pdn-relay-pin2 neigh-system))
         (sys (pdn-relay-pin1 neigh-system)))
        (t (error "This system (~a) does not appear to be connected to ~a!"
                  this-system neigh-system)))))


(defun connected-neighs (this-system &rest neighbors)
  "The function returns all systems which are currently neighbors of
this-system.  If any of the systems listed in neighbors argument is a
relay, then the state of that relay is evaluated and its other end is
added only if the relay is in state :closed.

Effectively, this function returns currently reachable neighbors of
this-system, taking into account systems which are connected via relays.

For usage of this, see how aircraft is constructed."
  (loop for neigh-system in neighbors
        when (find-connected-neighbor this-system (sys neigh-system))
        collect it))

