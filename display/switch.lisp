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


;;;
;;; Switch protocol
;;;

(defgeneric find-new-switch-position (switch mouse-button)
  (:documentation "Returns new switch position when given mouse button is
pressed."))


(defgeneric clicked-on-switch-p (switch x y)
  (:documentation "Returns true if click at coordinates x,y affects switch."))


(defgeneric find-active-image (switch)
  (:documentation "Returns SDL surface of the switch corresponding to its
current status.  This image is drawn when switch is displayed."))


;;;
;;; Data structures
;;;

(defmodel switch (family)
  ((x :cell nil
      :initarg :x
      :accessor switch-x)
   (y :cell nil
      :initarg :y
      :accessor switch-y)
   (width :cell nil
          :reader switch-width
          :initarg :width)
   (height :cell nil
           :reader switch-height
           :initarg :height)
   (model-object :cell nil
                 :initarg :model-object
                 :accessor model-object
                 :documentation "Underlying object which represents real
switch in the model.")))


(defmodel rotational-switch (switch)
  ((states :cell nil
           :initarg :states
           :reader switch-states)
   (images :cell nil
           :initarg :images
           :reader switch-images))
  (:default-initargs
      :width 72 :height 69))


(defmodel off/auto/on-switch (rotational-switch)
  ()
  (:default-initargs
      :states '(:off :auto :on)
      :images '(:p-off :p-auto :p-on))
  (:documentation "Rotational switch with 3 positions, OFF, AUTO and ON."))


(defmodel apu-start-switch (rotational-switch)
  ()
  (:default-initargs
      :states '(:off :on :start)
    :images '(:p-off :p-auto :p-on)))


(defmodel two-state-pushbutton-with-led (switch)
  ((states :cell nil
           :initarg :states
           :reader pb-states)
   (led-states :cell nil
               :initarg :led-states
               :reader pb-led-states)
   (images :cell nil
           :initarg :images
           :reader pb-images)
   (led-status :initarg :led-status
               :initform :off
               :accessor pb-led-status))
  (:default-initargs
      :width 59 :height 58)
  (:documentation "Push-button switch.  Has two positions
and an indication LED which can be :on or :off."))


(defmodel on/off-pushbutton (two-state-pushbutton-with-led)
  ()
  (:default-initargs
      :states '(:off :on)
    :led-states '(:off :on)
    :images '(:pb-blank :pb-in-on :pb-in-off :pb-on-off)))


(defmodel auto/isln-pushbutton (two-state-pushbutton-with-led)
  ()
  (:default-initargs
      :states '(:off :auto)
    :led-states '(:off :isln)
    :images '(:pb-blank :pb-in-auto :pb-in-isln :pb-auto-isln)))


(defmodel on/avail-pushbutton (two-state-pushbutton-with-led)
  ()
  (:default-initargs
      :states '(:off :on)
    :led-states '(:off :avail)
    :images '(:pb-blank :pb-in-on :pb-in-avail :pb-on-avail)))

;;;
;;; Protocol implementation for various switches
;;;

(defun in-rectangle-p (x y start-x start-y width height)
  "Returns true if point with coords \(x,y\) lies within rectangle whose
upper left corner has coordinates (start-x,start-y\) and whose width and
height are as specified."
  (and (and (> x start-x) (< x (+ start-x width)))
       (and (> y start-y) (< y (+ start-y height)))))


(defmethod clicked-on-switch-p ((switch switch) x y)
  (in-rectangle-p x y
                  (switch-x switch)
                  (switch-y switch)
                  (switch-width switch)
                  (switch-height switch)))


(defmethod handle-mouse-click ((switch switch) x y button)
  (when (clicked-on-switch-p switch x y)
    (logdebug "Mouse button ~a clicked, and dealing with it in ~a."
              button switch)
    (setf (xpc:switch-position (model-object switch))
          (find-new-switch-position switch button))
    (logdebug "Changed switch ~a to ~a." switch (xpc:switch-position (model-object switch)))
    (display-component switch)))


(defmethod display-component ((switch switch))
  (let ((surface (find-active-image switch)))
    (sdl:set-point-* surface
                     :x (switch-x switch)
                     :y (switch-y switch))
    (sdl:blit-surface surface)))


;;;
;;; Rotational switch functionality (e.g. pass signs switch,
;;; APU start switch, etc.)
;;;
(defmethod find-active-image ((switch rotational-switch))
  (image-surface (elt (switch-images switch)
                      (position (xpc:switch-position (model-object switch))
                                (switch-states switch)))))


(defmethod find-new-switch-position ((switch rotational-switch) mouse-button)
  (let* ((all-positions (switch-states switch))
         (index (position (xpc:switch-position (model-object switch))
                          all-positions)))
    (cond
      ((eql *mouse-left-button* mouse-button) (elt all-positions
                                                   (max 0 (1- index))))
      ((eql *mouse-right-button* mouse-button) (elt all-positions
                                                    (min (1- (length all-positions))
                                                         (1+ index)))))))


;;;
;;; Two state pushbuttons.
;;;


(defmethod find-active-image ((pb two-state-pushbutton-with-led))
  (flet ((num (state states)
           (if (eql state (first states)) 0 1)))
    (let ((images (mapcar #'image-surface (pb-images pb)))
          (pos-num (num (xpc:switch-position (model-object pb)) (pb-states pb)))
          (led-num (num (pb-led-status pb) (pb-led-states pb))))
      #|(logdebug "active image pos: ~a" (+ pos-num (* 2 led-num)))|#
      (elt images (+ pos-num (* 2 led-num))))))


(defmethod find-new-switch-position ((pb two-state-pushbutton-with-led)
                                     mouse-button)
  (xpc:the-other-value (xpc:switch-position (model-object pb))
                       (pb-states pb)))


;;;
;;; AUTO/ISLN variant of on/off pb switch
;;;

(defmodel auto/isln-pb-switch (on/off-pb-switch)
  ())

