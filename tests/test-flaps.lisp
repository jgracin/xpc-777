(in-package :xpc)


(eval-when (:load-toplevel :compile-toplevel :execute)
  (use-package :it.bese.fiveam))


(in-suite xpc-suite)


(test simple-flaps-test
  "Simple flaps test."
  (initialize-simulator)
  (set-flaps 30)
  (increase-clock-time 1000.0)
  (is (eql (current-flaps-position) 30)))
