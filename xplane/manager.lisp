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

(in-package :xplane)


;;; Constants, parameters, variables...

(defclass xplane-simulator ()
  ((ip-address :initarg :ip-address
               :initform #(127 0 0 1)
               :accessor simulator-ip-address
               )
   (port :initarg :port
         :initform 49000
         :accessor simulator-port)
   (socket :initform nil
           :accessor simulator-socket
           :documentation "A socket bound to the above address and port,
and which is used to send messages to the simulator.")))


(defvar *xplane-simulator* (make-instance 'xplane-simulator)
  "Default instance of a simulator")


(defvar *our-ip-address* #(127 0 0 1)
  "Address from which we send messages to the simulator.")


(defvar *our-udp-port* 32100
  "Port from which we send messages to the simulator and on which we
receive data from the simulator.")


(defun initialize-connection (simulator)
  (let* ((socket (make-instance 'inet-socket
                                :type :datagram :protocol :udp))) 
    (when (simulator-socket simulator)
      (error "The simulator already has a socket!  Close the connection
first and then call us again.  \(Hint: use close-connection\)"))
    (socket-bind socket *our-ip-address* *our-udp-port*)
    (setf (simulator-socket simulator) socket)))


(defun close-connection (simulator)
  (socket-close (simulator-socket simulator))
  (setf (simulator-socket simulator) nil))


;;; Implementation


(defun print-data-message (message)
  (when (string= "DATA" (parse-type message)) 
    (logdebug "DATA packet: ii: ~a, values:~%~{~a~%~}~%"
              (parse-internal-index-byte message)
              (parse-data-msg-values message))))


(defparameter *default-rcv-buffer-size* 512)

(defun make-receive-buffer (&optional (size *default-rcv-buffer-size*))
  (make-array size :element-type '(unsigned-byte 8)
              :adjustable t
              :fill-pointer *default-rcv-buffer-size*))


(defun receive-message (&optional (simulator *xplane-simulator*) buffer)
  "Reads one message from the simulator and stores it into the supplied
buffer.  If buffer is nil, creates a new buffer and returns it.  Supplied
buffer must be adjustable and must have a fill-pointer."
  (let ((buf (or buffer
                 (make-receive-buffer))))
    (multiple-value-bind (b len peer)
        (socket-receive (simulator-socket simulator)
                        buf
                        (buffer-size buf) :dontwait t)
      (if (null len)
          nil
          (progn
            (adjust-array buf (length buf) :fill-pointer len)
            (logdebug "received buffer: ~a" buf)
            buf)))))



(defun make-send-buffer ()
  (make-array 128 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))


(defun buffer-size (buffer)
  (array-dimension buffer 0))


(defun send-xpc-message (buffer &key (simulator *xplane-simulator*))
  (socket-send (simulator-socket simulator) buffer (length buffer)
               :address (list (simulator-ip-address simulator)
                              (simulator-port simulator))))


(defun create-data-msg (index-byte parameter-id value-list)
  (let ((buffer (make-send-buffer)))
    (write-prologue :data buffer :internal-index-byte index-byte)
    (write-xint parameter-id buffer)
    (loop for value in value-list
          do (write-xflt value buffer))
    (finalize-data-buffer buffer)
    buffer))



(defun set-throttle (&rest engine-values)
  (create-data-msg #x26 #x17 engine-values))

(defun interpret-true-or-false (value)
  (cond ((member value (list :yes :on :true)) t)
        ((and (floatp value) (> value 0.0)) t)
        ((member value (list 0.0 :no :off :false)) nil)
        (t (error "Unrecognized boolean value: ~a!  Expected something
like :yes, :on, :true, 1.0, etc."
                  value))))



(defun set-nav-light (value)
  (let ((float-value (if (interpret-true-or-false value) 1.0 0.0)))
    (create-data-msg 38 102 (list -999.0 float-value -999.0 -999.0 -999.0 -999.0 -999.0 -999.0 ))))


(defun set-lights (state &rest lights)
  "Creates a message for turning the lights on or off \(specified by
argument state which can be either :on or :off.\)"
  (let ((state-value (if (eql state :on)
                         1.0
                         0.0)))
    (create-data-msg 38 102
                     (list (if (member :avionics lights) state-value -999.0)
                           (if (member :nav lights) state-value -999.0)
                           (if (member :beacon lights) state-value -999.0)
                           (if (member :strobe lights) state-value -999.0)
                           (if (member :landing lights) state-value -999.0)
                           (if (member :taxi lights) state-value -999.0)
                           ;; What do nav-1 and nav-2 have to do with lights?
                           -999.0
                           -999.0))))
