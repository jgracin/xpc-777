(in-package :xpc)


(eval-when (:load-toplevel :compile-toplevel :execute)
  (use-package :it.bese.fiveam))


(in-suite xpc-suite)


(test simple-gear-test
  "Simple gear test."
  (initialize-simulator)
  (set-gear :down)
  (increase-clock-time 1000.0)
  (is (eql (current-gear-position) :down)))
