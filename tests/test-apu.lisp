(in-package :xpc)


(eval-when (:load-toplevel :compile-toplevel :execute)
  (use-package :it.bese.fiveam))


(in-suite xpc-suite)


(test apu-switch-from-start-to-on-automatically
  (initialize-simulator)
  (setf (switch-position (sys :apu-start-switch)) :start)
  (is (eql :start (switch-position (sys :apu-start-switch))))
  (increase-clock-time 10.0)
  (is (eql :on (switch-position (sys :apu-start-switch)))))


(test apu-switch-from-start-to-off
  "If APU start switch is momentarily moved to START position, and after
very short time (i.e. before it automatically goes back to ON) is moved to
OFF, it must stay in OFF and not move to ON."
  (initialize-simulator)
  (setf (switch-position (sys :apu-start-switch)) :start)
  (is (eql :start (switch-position (sys :apu-start-switch))))
  (increase-clock-time 0.1)
  ;; Check that it didn't come back to ON.
  (is (eql :start (switch-position (sys :apu-start-switch))))
  (setf (switch-position (sys :apu-start-switch)) :off)
  (is (eql :off (switch-position (sys :apu-start-switch))))
  ;; Ok, now wait and see if it moves.  It shouldn't.
  (increase-clock-time 100.0)
  (is (eql :off (switch-position (sys :apu-start-switch)))))
