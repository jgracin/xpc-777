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
;;; Main logic of electrical system.  Represents cummulative
;;; logic of various control units such as Bus Power Control
;;; Unit (BPCU), Generator Control Unit (GCU) etc.
;;;

(defun ecu-get-relay-state (relay)
  (values
   (let ((relay-state (gethash relay (ecu-relay-state-ht (sys :elec-control-unit)))))
     (or relay-state
         :open))
   (ecu-config-change-p (sys :elec-control-unit))))


(defun ecu-relay-closed-p (relay)
  (eql :closed (ecu-get-relay-state relay)))


(defun ecu-set-relay-state (relay state)
  (unless (ecu-config-change-p (sys :elec-control-unit))
    (with-integrity (:change)
      (setf (ecu-config-change-p (sys :elec-control-unit)) t)))
  (setf (gethash relay (ecu-relay-state-ht (sys :elec-control-unit)))
        state))


(defun ecu-l-main-ac-bus-directly-powered-p ()
  (and (elec-source-available-p (sys :l-idg))
       (ecu-relay-closed-p :l-gcb)))


(defun ecu-r-main-ac-bus-directly-powered-p ()
  (or (and (elec-source-available-p (sys :r-idg))
           (ecu-relay-closed-p :r-gcb))
      (and (elec-source-available-p (sys :pri-ext-pwr))
           (ecu-relay-closed-p :pri-epc))))


(defun ecu-set-switch-position (switch position)
  (with-integrity (:change)
    (setf (switch-position (sys switch)) position)))


(defun ecu-reconfigure-tie-bus ()
  (flet ((set-btbs (left right)
           (ecu-set-relay-state :l-btb (if (eql :auto (switch-position (sys :l-bus-tie-switch)))
                                           left
                                           :open))
           (ecu-set-relay-state :r-btb (if (eql :auto (switch-position (sys :r-bus-tie-switch)))
                                           right
                                           :open))))
    (if (ecu-l-main-ac-bus-directly-powered-p)
        (if (ecu-r-main-ac-bus-directly-powered-p)
            (set-btbs :open :open)
            (if (or (ecu-relay-closed-p :apb)
                    (ecu-relay-closed-p :sec-epc))
                (set-btbs :open :closed)
                (set-btbs :closed :closed)))
        (if (ecu-r-main-ac-bus-directly-powered-p)
            (if (or (ecu-relay-closed-p :apb)
                    (ecu-relay-closed-p :sec-epc))
                (set-btbs :closed :open)
                (if (and (eql :on (switch-position (sys :apu-gen-switch)))
                         (elec-source-available-p (sys :apu-gen)))
                    (progn
                      (ecu-set-relay-state :apb :closed)
                      (set-btbs :closed :open))
                    (set-btbs :closed :closed)))
            (set-btbs :closed :closed)))))


(defun ecu-handle-switch-change (switch new-position old-position)
  "Handles an event of changing electrical switch position.  These events
affect electrical source configuration."
  (declare (ignore old-position))
  (logdebug "running ecu-handle-switch-change")
  (case (system-name switch)
    (:r-gen-ctrl-switch (if (eql :on new-position)
                            (progn
                              (when (elec-source-available-p (sys :r-idg))
                                (ecu-set-relay-state :r-gcb :closed))
                              (ecu-set-relay-state :pri-epc :open)
                              (ecu-set-switch-position :pri-ext-pwr-switch :off))
                            (progn
                              (ecu-set-relay-state :r-gcb :open)
                              (when (and (elec-source-available-p (sys :apu-gen))
                                         (eql :on (switch-position (sys :apu-gen-switch))))
                                (ecu-set-relay-state :apb :closed)))))
    (:l-gen-ctrl-switch (if (eql :on new-position)
                            (progn
                              (when (elec-source-available-p (sys :l-idg))
                                (ecu-set-relay-state :l-gcb :closed))
                              (ecu-set-relay-state :sec-epc :open)
                              (ecu-set-switch-position :sec-ext-pwr-switch :off))
                            (ecu-set-relay-state :l-gcb :open)))
    (:apu-gen-switch (if (eql :on new-position)
                         (if (or (not (ecu-l-main-ac-bus-directly-powered-p))
                                 (not (ecu-r-main-ac-bus-directly-powered-p)))
                             (progn
                               (ecu-set-relay-state :apb :closed)
                               (ecu-set-relay-state :sec-epc :open)
                               (ecu-set-switch-position :sec-ext-pwr-switch :off)))
                         (ecu-set-relay-state :apb :open)))
    (:l-bu-gen-switch t)
    (:r-bu-gen-switch t)
    (:l-drive-disc-switch t)
    (:r-drive-disc-switch t)
    (:pri-ext-pwr-switch (if (eql :on new-position)
                             (if (elec-source-available-p (sys :pri-ext-pwr))
                                 (progn
                                   (ecu-set-relay-state :r-gcb :open)
                                   (ecu-set-relay-state :pri-epc :closed))
                                 (ecu-set-switch-position :pri-ext-pwr-switch :off))
                             (progn
                               (ecu-set-relay-state :pri-epc :open)
                               (if (and (elec-source-available-p (sys :r-idg))
                                        (eql :on (switch-position (sys :r-gen-ctrl-switch)))) 
                                   (ecu-set-relay-state :r-gcb :closed)))))
    (:sec-ext-pwr-switch (if (eql :on new-position)
                             (if (not (elec-source-available-p (sys :sec-ext-pwr)))
                                 (ecu-set-switch-position :sec-ext-pwr-switch :off)
                                 (progn
                                   (ecu-set-relay-state :apb :open)
                                   (ecu-set-relay-state :sec-epc :closed)
                                   (ecu-set-relay-state :l-gcb :open)))
                             (progn
                               (when (elec-source-available-p (sys :apu-gen))
                                 (ecu-set-relay-state :apb :closed))
                               (ecu-set-relay-state :sec-epc :open)))))
  (ecu-reconfigure-tie-bus))


(defun ecu-handle-source-available-change (gen avail-p-new-state avail-p-old-state)
  "Handles the event of one of the generators coming online or offline."
  (declare (ignore avail-p-old-state))
  (case (system-name gen)
    (:l-idg (if (not avail-p-new-state)
                (ecu-set-relay-state :l-gcb :open)
                (progn
                  (ecu-set-relay-state :l-gcb :closed)
                  (ecu-set-relay-state :sec-epc :open))))
    (:r-idg (if (not avail-p-new-state)
                (ecu-set-relay-state :r-gcb :open)
                (progn
                  (ecu-set-relay-state :r-gcb :closed)
                  (ecu-set-relay-state :pri-epc :open))))
    (:apu-gen (if (not avail-p-new-state)
                  (ecu-set-relay-state :apb :open))))
  (ecu-reconfigure-tie-bus))




