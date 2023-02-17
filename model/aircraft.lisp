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


(defmodel aircraft (family)
  ((name :initarg :name :initform nil :reader aircraft-name)))


(defmodel b777-aircraft (aircraft)
  ()
  (:default-initargs
      :name "Boeing 777-200")
  (:documentation "Boeing 777-200 aircraft."))


(defparameter *b777-200*
  (make-system
   b777-aircraft
   :aircraft
   :subsystems
   (flight-parameters :flight-parameters)
   (engine-parameters :l-eng-params)
   (engine-parameters :r-eng-params)
   (engine-parameters :apu-eng-params)
   (lights :lights
           :subsystems
           (switch :pass-signs-switch)
           (switch :no-smoking-signs-switch)
           (light :pass-signs-light
                  :status (c_? (b777-pass-signs-light-logic self)))
           (light :no-smoking-signs-light
                  :status (c_? (b777-no-smoking-signs-light-logic self))))
   (b777-flaps-system :flaps-system
                      :subsystems
                      (b777-flaps-lever :flaps-lever)
                      (flaps-indicator :flaps-indicator)
                      (b777-flaps-surface :left-inboard-flap)
                      (b777-flaps-surface :right-inboard-flap)
                      (b777-flaps-surface :left-outboard-flap)
                      (b777-flaps-surface :right-outboard-flap))
   (b777-gear-system :gear-system
                     :subsystems
                     (gear-lever :gear-lever)
                     (gear-indicator :gear-indicator)
                     (b777-gear :nose-gear)
                     (b777-gear :left-main-gear)
                     (b777-gear :right-main-gear))
   (b777-elec-system :elec-system 
                     :subsystems
                     ;; control unit, stores data needed in the implementation
                     ;; of control logic
                     (elec-control-unit :elec-control-unit)
                     ;; switches
                     (elec-switch :main-battery-switch)
                     (elec-switch :apu-start-switch
                       :change-handler 'apu-start-switch-change-handler)
                     (elec-switch :apu-gen-switch)
                     (elec-switch :l-bus-tie-switch :switch-position (c-in :auto))
                     (elec-switch :r-bus-tie-switch :switch-position (c-in :auto))
                     (elec-switch :r-gen-ctrl-switch)
                     (elec-switch :l-gen-ctrl-switch)
                     (elec-switch :l-drive-disc-switch)
                     (elec-switch :r-drive-disc-switch)
                     (elec-switch :l-bu-gen-switch)
                     (elec-switch :r-bu-gen-switch)
                     (elec-switch :pri-ext-pwr-switch)
                     (elec-switch :sec-ext-pwr-switch)
                     (elec-switch :ife-switch)
                     (elec-switch :utility-switch)
                     ;; electrical generators/sources
                     (elec-generator :l-idg
                                     :nominal-power *idg-power*
                                     :nominal-voltage *idg-voltage*
                                     :engine-params :l-eng-params
                                     :power (c_? (engine-driven-gen-output self :power))
                                     :voltage (c_? (engine-driven-gen-output self :voltage))
                                     :neighs (c_? (connected-neighs self :l-gcb))
                                     :available-p (c_? (gen-running-p :l-idg *idg-voltage*)))
                     (elec-generator :r-idg
                                     :nominal-power *idg-power*
                                     :nominal-voltage *idg-voltage*
                                     :engine-params :r-eng-params
                                     :power (c_? (engine-driven-gen-output self :power))
                                     :voltage (c_? (engine-driven-gen-output self :voltage))
                                     :neighs (c_? (connected-neighs self :r-gcb))
                                     :available-p (c_? (gen-running-p :r-idg *idg-voltage*)))
                     (elec-generator :apu-gen
                                     :nominal-power *apu-gen-power*
                                     :nominal-voltage *apu-gen-voltage*
                                     :engine-params :apu-eng-params
                                     :power (c_? (engine-driven-gen-output self :power))
                                     :voltage (c_? (engine-driven-gen-output self :voltage))
                                     :neighs (c_? (connected-neighs self :tie-bus))
                                     :available-p (c_? (gen-running-p :apu-gen *apu-gen-voltage*)))
                     (elec-source :pri-ext-pwr
                                  :power (c_? (ext-pwr-power self))
                                  :voltage (c_? (ext-pwr-voltage self))
                                  :nominal-power 90000
                                  :nominal-voltage 115
                                  :neighs (c_? (connected-neighs self :pri-epc))
                                  :available-p (c-in t))
                     (elec-source :sec-ext-pwr
                                  :power (c_? (ext-pwr-power self))
                                  :voltage (c_? (ext-pwr-voltage self))
                                  :nominal-power 90000
                                  :nominal-voltage 115
                                  :neighs (c_? (connected-neighs self :sec-epc))
                                  :available-p (c-in t))
                     (elec-generator :l-bu-gen 
                                     :nominal-power *bu-gen-power*
                                     :nominal-voltage *bu-gen-voltage*
                                     :engine-params :l-eng-params
                                     :power (c_? (engine-driven-gen-output self :power))
                                     :voltage (c_? (engine-driven-gen-output self :voltage))
                                     :available-p (c_? (gen-running-p :l-bu-gen *bu-gen-voltage*)))
                     (elec-generator :r-bu-gen
                                     :nominal-power *bu-gen-power*
                                     :nominal-voltage *bu-gen-voltage*
                                     :engine-params :r-eng-params
                                     :power (c_? (engine-driven-gen-output self :power))
                                     :voltage (c_? (engine-driven-gen-output self :voltage))
                                     :available-p (c_? (gen-running-p :r-bu-gen *bu-gen-voltage*)))
                     (elec-generator :rat-gen :power (c? 0) :voltage (c? 0))
                     ;; batteries, capacity in Wh
                     (battery-source :main-battery
                                     :nominal-power 20000
                                     :nominal-voltage 28
                                     :power 20000
                                     :voltage (c? 28)
                                     :capacity (c? (* 28 47))
                                     :neighs (c_? (connected-neighs self :hot-bat-bus)))
                     (battery-source :apu-battery
                                     :nominal-power 20000
                                     :nominal-voltage 28
                                     :power 20000
                                     :voltage (c? 28)
                                     :capacity (c? (* 28 47)))
                     ;; AC busses
                     (elec-bus :l-main-ac-bus
                               :neighs (c_? (connected-neighs self
                                                              :l-gcb
                                                              :l-btb
                                                              :l-util-bus
                                                              :l-tbb)))
                     (elec-bus :r-main-ac-bus
                               :neighs (c_? (connected-neighs self
                                                              :r-gcb
                                                              :r-btb
                                                              :r-util-bus
                                                              :r-tbb
                                                              :pri-epc)))
                     (elec-bus :tie-bus
                               :neighs (c_? (connected-neighs self
                                                              :l-btb
                                                              :r-btb
                                                              :apb
                                                              :sec-epc)))
                     (elec-bus :l-xfr-bus
                               :neighs (c_? (connected-neighs self
                                                              :l-tbb
                                                              :l-ccb)))
                     (elec-bus :r-xfr-bus
                               :neighs (c_? (connected-neighs self
                                                              :r-tbb
                                                              :r-ccb)))
                     (elec-bus :xfr-tie-bus
                               :neighs (c_? (connected-neighs self
                                                              :l-ccb
                                                              :r-ccb)))
                     (elec-bus :gh-ac-bus)
                     (elec-bus :gnd-svc-bus)
                     (elec-bus :l-util-bus
                               :neighs (c_? (connected-neighs self
                                                              :l-ubb)))
                     (elec-bus :r-util-bus
                               :neighs (c_? (connected-neighs self
                                                              :r-ubb)))
                                 
                     ;; DC busses
                     (elec-bus :apu-bat-bus)
                     (elec-bus :hot-bat-bus
                               :neighs (c_? (connected-neighs self :main-battery)))
                     (elec-bus :bat-bus)
                     (elec-bus :l-dc-bus)
                     (elec-bus :r-dc-bus)
                     (elec-bus :cpt-flt-inst-bus)
                     (elec-bus :f/o-flt-inst-bus)
                     (elec-bus :bat-bus-2)
                     ;; relays
                     (elec-relay :l-gcb
                                 :pin1 :l-main-ac-bus
                                 :pin2 :l-idg
                                 :state (c_? (ecu-get-relay-state :l-gcb)))
                     (elec-relay :r-gcb
                                 :pin1 :r-main-ac-bus
                                 :pin2 :r-idg
                                 :state (c_? (ecu-get-relay-state :r-gcb)))
                     (elec-relay :apb
                                 :pin1 :apu-gen
                                 :pin2 :tie-bus
                                 :state (c_? (ecu-get-relay-state :apb)))
                     (elec-relay :pri-epc
                                 :pin1 :pri-ext-pwr
                                 :pin2 :r-main-ac-bus
                                 :state (c_? (ecu-get-relay-state :pri-epc)))
                     (elec-relay :sec-epc
                                 :pin1 :sec-ext-pwr
                                 :pin2 :tie-bus
                                 :state (c_? (ecu-get-relay-state :sec-epc)))
                     (elec-relay :hot-bus&bat-bus-2-relay
                                 :pin1 :hot-bat-bus
                                 :pin2 :bat-bus-2)
                     (elec-relay :l-btb
                                 :pin1 :tie-bus
                                 :pin2 :l-main-ac-bus
                                 :state (c_? (ecu-get-relay-state :l-btb)))
                     (elec-relay :r-btb
                                 :pin1 :tie-bus
                                 :pin2 :r-main-ac-bus
                                 :state (c_? (ecu-get-relay-state :r-btb)))
                     (elec-relay :l-tbb
                                 :pin1 :l-xfr-bus
                                 :pin2 :l-main-ac-bus
                                 :state (c_? (ecu-get-relay-state :l-tbb)))
                     (elec-relay :r-tbb
                                 :pin1 :r-xfr-bus
                                 :pin2 :r-main-ac-bus
                                 :state (c_? (ecu-get-relay-state :r-tbb)))
                     (elec-relay :l-ccb
                                 :pin1 :l-xfr-bus
                                 :pin2 :xfr-tie-bus
                                 :state (c_? (ecu-get-relay-state :l-ccb)))
                     (elec-relay :r-ccb
                                 :pin1 :r-xfr-bus
                                 :pin2 :xfr-tie-bus
                                 :state (c_? (ecu-get-relay-state :r-ccb)))
                     ;; utility bus breakers, open only in case of load shedding
                     (elec-relay :l-ubb
                                 :pin1 :l-main-ac-bus
                                 :pin2 :l-util-bus
                                 :state (c_? (ecu-get-relay-state :l-ubb)))
                     (elec-relay :r-ubb
                                 :pin1 :r-main-ac-bus
                                 :pin2 :r-util-bus
                                 :state (c_? (ecu-get-relay-state :r-ubb)))
                     ;; The following two breakers don't really exist in
                     ;; real airplane, but we use them here instead of
                     ;; backup generator converter (which is not modeled).
                     ;;--------------------------------------------
                     (elec-relay :l-bu-gcb
                                 :pin1 :l-bu-gen
                                 :pin2 :xfr-tie-bus
                                 :state (c_? (ecu-get-relay-state :l-bu-gcb)))
                     (elec-relay :r-bu-gcb
                                 :pin1 :r-bu-gen
                                 :pin2 :xfr-tie-bus
                                 :state (c_? (ecu-get-relay-state :r-bu-gcb)))
                     ;;--------------------------------------------
                     (elec-relay :main-bat-relay
                                 :pin1 :hot-bat-bus
                                 :pin2 :bat-bus)
                     (elec-relay :bat-cpt-isln-relay
                                 :pin1 :bat-bus
                                 :pin2 :cpt-flt-inst-bus)
                     (elec-relay :dc-bus-tie-relay
                                 :pin1 :l-dc-bus
                                 :pin2 :r-dc-bus)
                     (elec-relay :cpt-f/o-bus-tie-relay
                                 :pin1 :cpt-flt-inst-bus
                                 :pin2 :f/o-flt-inst-bus)
                     (elec-relay-w/base :gnd-pwr-bat-relay
                                        :base-pin :bat-bus-2
                                        :pin1 :hot-bat-bus
                                        :pin2 :cpt-flt-inst-bus)
                     )))


(setq *default-aircraft* *b777-200*)


;;;
;;; Debugging utils
;;;

(defun print-system (system &optional (stream t))
  (let ((s (etypecase system
             (keyword (sys system))
             (family system))))
    (format stream "~a"
            (with-output-to-string
              (str)
              (print-unreadable-object (s str :type t :identity t)
                (format str "~a" (system-name s)))))))


(defun print-systems-cache ()
  (loginfo "Systems cache objects:")
  (loginfo
   (with-output-to-string
     (str)
     (loop for key being each hash-key in *systems-cache*
           using (hash-value value)
           for cnt from 0 
           do
           (when (zerop (mod cnt 2)) (format str "~%"))
           (format str "~50a" (print-system value nil)))))
  (values))
