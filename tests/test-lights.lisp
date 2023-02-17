(in-package :xpc)


(eval-when (:load-toplevel :compile-toplevel :execute)
  (use-package :it.bese.fiveam))


(in-suite xpc-suite)


(test pass-signs-with-switch-set-to-on-or-off
  "Tests pass signs functionality when pass-signs switch is set in ON or
OFF position."
  (let ((light (sys :pass-signs-light)))
    (initialize-simulator)
    (set-clean-configuration)
    ;; off position
    (setf (switch-position (sys :pass-signs-switch)) :off)
    (is (eql :off (light-status light)))
    (set-altitude 4000)
    (is (eql :off (light-status light)))
    (set-altitude 40000)
    (is (eql :off (light-status light)))
    (set-altitude 18000)
    (is (eql :off (light-status light)))
    (set-altitude -100)
    (is (eql :off (light-status light)))
    ;; on position
    (set-pass-signs :on)
    (is (eql :on (light-status light)))
    (set-altitude 4000)
    (is (eql :on (light-status light)))
    (set-altitude 40000)
    (is (eql :on (light-status light)))
    (set-altitude 18000)
    (is (eql :on (light-status light)))
    (set-altitude -100)
    (is (eql :on (light-status light)))
    ;; off again, just to make sure
    (set-pass-signs :off)
    (is (eql :off (light-status light)))
    (set-altitude 4000)
    (is (eql :off (light-status light)))
    (set-altitude 40000)
    (is (eql :off (light-status light)))
    (set-altitude 18000)
    (is (eql :off (light-status light)))
    (set-altitude -100)
    (is (eql :off (light-status light)))))

(test simple-pass-signs-test
  "Simple test of ON and OFF positions for light switch."
  (initialize-simulator)
  (let ((light (sys :pass-signs-light)))
    (set-clean-configuration)
    ;; off position
    (set-pass-signs :off)
    (is (eql :off (light-status light)))
    (set-altitude 4000)
    (is (eql :off (light-status light)))))


(test no-smoking-signs-light-and-switches
  "Tests no smoking signs functionality."
  (initialize-simulator)
  (let ((light (sys :no-smoking-signs-light)))
    (set-clean-configuration)
    (set-no-smoking-signs :off)
    (is (eql :off (light-status light)))
    (set-no-smoking-signs :on)
    (is (eql :on (light-status light)))
    (set-no-smoking-signs :off)
    (is (eql :off (light-status light)))
    (set-no-smoking-signs :auto)
    (is (eql :on (light-status light)))))


(test pass-signs-in-automatic-mode
  "Tests pass signs behavior when the switch is in position AUTO."
  (initialize-simulator)
  (let ((light (sys :pass-signs-light)))
    (set-clean-configuration)
    (increase-clock-time 100.0)
    ;; auto position
    (set-pass-signs :auto)
    (set-altitude 0)
    (is (eql :on (light-status light)))
    (set-altitude 4000)
    (is (eql :on (light-status light)))
    (set-altitude 18000)
    (is (eql :off (light-status light)))
    (set-altitude 39000)
    (is (eql :on (light-status light)))
    (set-altitude 1000)
    (is (eql :on (light-status light)))
    
    ;; Flaps 5 should turn on pass-lights unconditionally.
    ;; First make the lights switch off by setting high altitude.
    (set-altitude 18000)
    (is (eql :off (light-status light)))
    (set-flaps 5)
    (increase-clock-time 100.0)
    (is (eql :on (light-status light)))

    ;; Moving gear lever out of UP position
    ;; should turn on pass-lights unconditionally.
    ;; First make the lights switch off
    (set-pass-signs :off)
    (set-flaps 0)
    (increase-clock-time 100.0)
    (set-altitude 18000)
    (set-pass-signs :auto)
    (is (eql :off (light-status light)))
    (set-gear :down)
    (is (eql :on (light-status light)))))

