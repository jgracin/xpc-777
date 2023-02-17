(in-package :xpc)


(eval-when (:load-toplevel :compile-toplevel :execute)
  (use-package :it.bese.fiveam))


(in-suite xpc-suite)


(defun set-switch-position (switch position)
  (setf (switch-position (sys switch)) position))


(defun set-elec-switches (switch-position-list)
  (loop for (switch pos) in switch-position-list
        do (setf (switch-position (sys switch)) pos)))


(defun set-normal-elec-switch-configuration ()
  (set-elec-switches '((:l-bus-tie-switch :off)
                       (:r-bus-tie-switch :off)
                       (:apu-gen-switch :off)
                       (:l-gen-ctrl-switch :off)
                       (:r-gen-ctrl-switch :off) 
                       (:pri-ext-pwr-switch :off)
                       (:sec-ext-pwr-switch :off)))
  (set-elec-switches '((:l-bus-tie-switch :auto)
                       (:r-bus-tie-switch :auto)
                       (:apu-gen-switch :on)
                       (:l-gen-ctrl-switch :on)
                       (:r-gen-ctrl-switch :on))))


(defun verify-relay-positions (relay-state-list)
  (loop for (relay state) in relay-state-list
        do (is (eql state (pdn-relay-state (sys relay)))
               "~a expected to be in state ~a, and it wasn't" relay state)))


(defun verify-bus-has-power (bus)
  (is (plusp (elec-bus-power (sys bus)))))


(test btbs-all-working
  (set-engine-running-idle :apu-eng-params)
  (set-engine-running-idle :l-eng-params)
  (set-engine-running-idle :r-eng-params)
  (set-normal-elec-switch-configuration)
  (verify-relay-positions '((:l-btb :open)
                            (:r-btb :open))))


(test btbs-right-engine-and-apu
  (shutdown-engine :l-eng-params)
  (set-engine-running-idle :apu-eng-params)
  (set-engine-running-idle :r-eng-params)
  (set-normal-elec-switch-configuration)
  (verify-relay-positions '((:l-btb :closed)
                            (:r-btb :open)
                            (:apb :closed))))


(test btbs-left-engine-and-apu
  (shutdown-engine :r-eng-params)
  (set-engine-running-idle :apu-eng-params)
  (set-engine-running-idle :l-eng-params)
  (set-normal-elec-switch-configuration)
  (verify-relay-positions '((:r-btb :closed)
                            (:l-btb :open)
                            (:apb :closed))))


(test btbs-only-on-apu
  (shutdown-engine :l-eng-params)
  (shutdown-engine :r-eng-params)
  (set-engine-running-idle :apu-eng-params)
  (set-normal-elec-switch-configuration)
  (verify-relay-positions '((:r-btb :closed)
                            (:l-btb :closed)
                            (:apb :closed)))
  (verify-bus-has-power :l-main-ac-bus)
  (verify-bus-has-power :r-main-ac-bus))


(test btbs-left-bus-tie-switch
  (set-engine-running-idle :apu-eng-params)
  (set-engine-running-idle :l-eng-params)
  (set-engine-running-idle :r-eng-params)
  (set-normal-elec-switch-configuration)
  (verify-relay-positions '((:l-btb :open)
                            (:r-btb :open)))
  (verify-bus-has-power :l-main-ac-bus)
  ;; now start playing with switches
  (setf (switch-position (sys :l-bus-tie-switch)) :off)
  (verify-relay-positions '((:l-btb :open)
                            (:r-btb :open)))
  (verify-bus-has-power :l-main-ac-bus)
  ;; 
  (setf (switch-position (sys :l-bus-tie-switch)) :auto)
  (verify-relay-positions '((:l-btb :open)
                            (:r-btb :open)))
  ;;
  (setf (switch-position (sys :l-gen-ctrl-switch)) :off)
  (verify-relay-positions '((:l-btb :closed)
                            (:apb :closed)
                            (:r-btb :open)))
  (verify-bus-has-power :l-main-ac-bus)
  ;;
  (setf (switch-position (sys :l-gen-ctrl-switch)) :on)
  (verify-relay-positions '((:l-btb :open)
                            (:r-btb :open)))
  (setf (switch-position (sys :l-gen-ctrl-switch)) :off)
  (setf (switch-position (sys :apu-gen-switch)) :off)
  (verify-relay-positions '((:l-btb :closed)
                            (:r-btb :closed)
                            (:apb :open))))


(test pri-epc-test
  (set-engine-running-idle :apu-eng-params)
  (set-engine-running-idle :l-eng-params)
  (set-engine-running-idle :r-eng-params)  
  (set-normal-elec-switch-configuration)
  (set-elec-switches '((:r-bus-tie-switch :off)
                       (:apu-gen-switch :off)
                       (:pri-ext-pwr-switch :on)
                       (:r-gen-ctrl-switch :on)
                       (:sec-ext-pwr-switch :off)
                       (:l-bus-tie-switch :auto)
                       (:l-gen-ctrl-switch :on)))
  (verify-relay-positions '((:pri-epc :closed)
                            (:r-btb :open))))


(test sec-epc-test
  (set-engine-running-idle :apu-eng-params)
  (set-engine-running-idle :l-eng-params)
  (set-engine-running-idle :r-eng-params)  
  (set-normal-elec-switch-configuration)
  (setf (switch-position (sys :sec-ext-pwr-switch)) :on)
  (verify-relay-positions '((:sec-epc :closed)
                            (:apb :open)
                            (:l-btb :closed)
                            (:r-btb :open)
                            (:l-gcb :open)))
  (setf (switch-position (sys :sec-ext-pwr-switch)) :off)
  (verify-relay-positions '((:sec-epc :open)
                            (:apb :closed)
                            (:l-btb :closed)
                            (:l-gcb :open)
                            (:r-btb :open)))
  (shutdown-engine :apu-eng-params)
  (verify-relay-positions '((:sec-epc :open)
                            (:apb :open)
                            (:l-btb :closed)
                            (:r-btb :closed)))
  (shutdown-engine :l-eng-params)
  (verify-relay-positions '((:sec-epc :open)
                            (:apb :open)
                            (:l-btb :closed)
                            (:r-btb :closed)))
  (verify-bus-has-power :l-main-ac-bus))



(test on-both-ext-srcs-and-then-engine-start
  "Tests what happens when aircraft is powered with both external sources
and right engine is started."
  (shutdown-engine :apu-eng-params)
  (shutdown-engine :l-eng-params)
  (shutdown-engine :r-eng-params)
  (setf (elec-source-available-p (sys :pri-ext-pwr)) t)
  (setf (elec-source-available-p (sys :sec-ext-pwr)) t)
  (set-normal-elec-switch-configuration)
  (setf (switch-position (sys :pri-ext-pwr-switch)) :on)
  (setf (switch-position (sys :sec-ext-pwr-switch)) :on)
  (verify-relay-positions '((:pri-epc :closed)
                            (:sec-epc :closed)
                            (:apb :open)
                            (:l-btb :closed)
                            (:r-btb :open)))
  (verify-bus-has-power :l-main-ac-bus)
  (verify-bus-has-power :r-main-ac-bus)
  (set-engine-running-idle :r-eng-params)
  (verify-relay-positions '((:pri-epc :open)
                            (:sec-epc :closed)
                            (:apb :open)
                            (:l-btb :closed)
                            (:r-btb :open)))
  (set-engine-running-idle :l-eng-params)
  (verify-relay-positions '((:pri-epc :open)
                            (:sec-epc :open)
                            (:apb :open)
                            (:l-btb :open)
                            (:r-btb :open))))


(test apu-gen-coming-online
  "Tests changes when APU is started."
  (shutdown-engine :apu-eng-params)
  (shutdown-engine :l-eng-params)
  (shutdown-engine :r-eng-params)
  (set-normal-elec-switch-configuration)
  ;; TODO
  )

(test gen-ctrl-switches-reset
  "Tests that generator control switches disengage respective external
power sources."
  (shutdown-engine :apu-eng-params)
  (shutdown-engine :l-eng-params)
  (shutdown-engine :r-eng-params)
  (set-normal-elec-switch-configuration)
  (setf (elec-source-available-p (sys :pri-ext-pwr)) t)
  (setf (elec-source-available-p (sys :sec-ext-pwr)) t)
  (set-switch-position :pri-ext-pwr-switch :on)
  (set-switch-position :sec-ext-pwr-switch :on)
  (verify-relay-positions '((:pri-epc :closed)
                            (:sec-epc :closed)
                            (:apb :open)
                            (:l-btb :closed)
                            (:r-btb :open)))
  (set-switch-position :r-gen-ctrl-switch :off)
  (set-switch-position :r-gen-ctrl-switch :on)
  (verify-relay-positions '((:pri-epc :open)
                            (:sec-epc :closed)
                            (:apb :open)
                            (:l-btb :closed)
                            (:r-btb :closed
                             )))
  ;; back to two ext power configuration
  (set-switch-position :pri-ext-pwr-switch :on)
  ;; not try left gen-ctrl switch
  (set-switch-position :l-gen-ctrl-switch :off)
  (set-switch-position :l-gen-ctrl-switch :on)
  (verify-relay-positions '((:pri-epc :closed)
                            (:sec-epc :open)
                            (:apb :open)
                            (:l-btb :closed)
                            (:r-btb :closed))))
