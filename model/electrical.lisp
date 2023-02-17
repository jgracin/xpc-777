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
;;; Constants, globals
;;;

(defparameter *idg-power* 120000)

(defparameter *idg-voltage* 115)

(defparameter *apu-gen-power* 120000)

(defparameter *apu-gen-voltage* 115)

(defparameter *main-bat-power* 20000)

(defparameter *main-bat-voltage* 28)

(defparameter *ext-source-power* 90000)

(defparameter *ext-source-voltage* 115)

(defparameter *bu-gen-power* 20000)

(defparameter *bu-gen-voltage* 115)

(defparameter *rat-gen-power* 7500)

(defparameter *rat-get-voltage* 48)


;;;
;;; Model
;;;

(defmodel elec-system (system) ())


(defmodel b777-elec-system (elec-system) ())


(defmodel elec-component (pdn-component)
  ())


(defmodel elec-control-unit (elec-component)
  ((config-change-p :cell :ephemeral
                    :initform (c-in nil)
                    :accessor ecu-config-change-p)
   (relay-state-ht :cell nil
                   :initarg :relay-state-ht
                   :accessor ecu-relay-state-ht
                   :documentation "Maps relay name to its current state."))
  (:default-initargs
      :relay-state-ht (make-hash-table)))


(defmodel elec-switch (switch) ())


(defmodel elec-bus (elec-component pdn-bus)
  ())


(defmodel elec-relay (elec-component pdn-relay)
  ())


(defmodel elec-relay-w/base (elec-component pdn-relay-w/base)
  ())


(defmodel elec-source (elec-component pdn-source)
  ((nominal-power :cell nil
                  :initarg :nominal-power
                  :reader elec-source-nominal-power
                  :documentation "Constant.  Declared power which
source is capable of providing in normal operation.")
   (nominal-voltage :cell nil
                    :initarg :nominal-voltage
                    :reader elec-source-nominal-voltage)
   (power :initarg :power
          :accessor elec-source-power
          :documentation "Current power of the source, in Watts.  This can
be less then the nominal power, in case of source functional degradation.")
   (voltage :initarg :voltage
	    :accessor elec-source-voltage
	    :documentation "Current voltage provided by the source.  Can
be less then the nominal voltage, e.g. during high loads.")
   (available-p :cell t
                :initarg :available-p
                :accessor elec-source-available-p
                :documentation "Whether source is providing power or not.
This is used as a trigger for systems which react when power from
sources becomes available."))
  (:default-initargs
      :available-p (c? nil)))
(export 'elec-source-available-p)


(defmodel elec-generator (elec-source)
  ((engine-params :cell nil
                  :initarg :engine-params
                  :reader elec-generator-engine-params
                  :documentation "Engine which drives this generator, represented
by its parameters.")))


(defmodel elec-converter (pdn-converter)
  ((output-voltage :cell nil
                   :initarg :output
                   :accessor elec-tru-output-voltage))
  (:documentation "Converts AC->DC or DC->AC."))


(defmodel elec-tru (elec-converter)
  ()
  (:documentation "Transformer rectifier unit.  Input is AC source, output
is DC bus or load."))


(defmodel elec-inverter (elec-converter)
  ()
  (:documentation "Input is DC, output is AC."))


(defmodel battery-source (elec-source)
  ((capacity :initarg :capacity
             :accessor battery-capacity)
   (discharging-p :initarg :discharging-p
                  :accessor battery-discharging-p))
  (:default-initargs
      :discharging-p (c-in nil)))


;;;
;;; APU
;;;

(defun apu-start-switch-change-handler (self new-position old-position)
  (logdebug "APU start switch moved from ~a to ~a." old-position new-position)
  (when (or (eql :on new-position)
            (eql :off new-position))
    (xpc:delete-actions-by-tag :apu-switch-spring-load))
  (when (eql :start new-position)
    (schedule 1.5
              (lambda ()
                (with-integrity (:change)
                  (setf (switch-position self) :on)))
              :tag :apu-switch-spring-load)))


;;;
;;; Bus voltage and power are calculated on demand.
;;;

(defgeneric-public elec-bus-voltage (bus)
  (:documentation "Returns the current voltage available on the bus."))


(defgeneric-public elec-bus-power (bus)
  (:documentation "Returns the available power on the bus."))


(defmethod elec-bus-voltage (bus)
    (let ((sources (find-pdn-sources bus)))
    (if sources
        (elec-source-voltage (first sources))
        0)))


(defmethod elec-bus-power (bus)
  (let ((sources (find-pdn-sources bus)))
    (if sources
        (elec-source-power (first sources))
        0)))


(defun-public reset-elec-system ()
  "Sets all switches, breakers etc. to their standard ground, pre-flight
position."
  ;; switches
  (setf (switch-position (sys :l-bus-tie-switch)) :auto)
  (setf (switch-position (sys :r-bus-tie-switch)) :auto)
  (setf (switch-position (sys :l-gen-ctrl-switch)) :on)
  (setf (switch-position (sys :r-gen-ctrl-switch)) :on)
  (setf (switch-position (sys :apu-gen-switch)) :on)
  (setf (switch-position (sys :l-bu-gen-switch)) :on)
  (setf (switch-position (sys :r-bu-gen-switch)) :on))


;;;
;;; External power
;;;
(defun ext-pwr-power (ext-pwr)
  (if (elec-source-available-p (sys ext-pwr))
      (elec-source-nominal-power (sys ext-pwr))
      0))


(defun ext-pwr-voltage (ext-pwr)
  (if (elec-source-available-p (sys ext-pwr))
      (elec-source-nominal-voltage (sys ext-pwr))
      0))

;;;
;;; IDGs
;;;
(defun gen-running-p (gen required-voltage)
  (>= (elec-source-voltage (sys gen))
      required-voltage))


(defun engine-driven-gen-output (generator power-or-voltage)
  (if (> (engine-n2 (sys (elec-generator-engine-params generator)))
         50)
      (ecase power-or-voltage
        (:power (elec-source-nominal-power generator))
        (:voltage (elec-source-nominal-voltage generator)))
      0))

;;;
;;; Debugging functions
;;;

(defun print-elec-system-status ()
  (flet ((remove-suffix-name (suffix system)
           (let* ((system-name (symbol-name (system-name system)))
                  (suffix-position (search suffix (string-downcase system-name))))
             (subseq system-name
                     0 (or suffix-position (length system-name))))))
    (let* ((systems (kids (sys :elec-system)))
           (relays (remove-if-not (lambda (s) (typep s 'elec-relay))
                                  systems))
           (sources (remove-if-not (lambda (s) (typep s 'elec-source))
                                   systems))
           (buses (remove-if-not (lambda (s) (typep s 'elec-bus))
                                 systems))
           (switches (remove-if-not (lambda (s) (typep s 'switch))
                                    systems))) 
      (loginfo
       (with-output-to-string
         (str)
         (format str "============Electrical system status:===============")
         (format str "~&---------------- R E L A Y S ---------------------------------------")
         (loop for relay in relays
               for cnt from 0
               when (zerop (mod cnt 4)) do (format str "~%")
               do (format str " |~18@a:~7@a"
                          (remove-suffix-name "-relay" relay)
                          (pdn-relay-state relay)))
         (format str "~&---------------- S O U R C E S -------------------------------------")
         (loop for source in sources
               for cnt from 0
               when (zerop (mod cnt 3)) do (format str "~%")
               do (format str " |~12@a:~7@aW,~5@aV"
                          (remove-suffix-name "-source" source)
                          (elec-source-power source)
                          (elec-source-voltage source)))
         (format str "~&----------------- B U S E S ----------------------------------------")
         (loop for bus in buses
               for cnt from 0
               when (zerop (mod cnt 4)) do (format str "~%")
               do (format str " |~12@a:~7@aW,~3@aV"
                          (remove-suffix-name "-bus" bus)
                          (elec-bus-power bus)
                          (elec-bus-voltage bus)))
         (format str "~&------------------ S W I T C H E S ---------------------------------")
         (loop for switch in switches
               for cnt from 0
               when (zerop (mod cnt 5)) do (format str "~%")
               do (format str " |~12@a: ~5a"
                          (remove-suffix-name "-switch" switch)
                          (switch-position switch)))))
      (loginfo "=============End of elec system status==============")))
  (values))
