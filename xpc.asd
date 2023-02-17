;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(in-package :cl-user)
 
(defpackage :xpc-system
    (:use :cl :asdf))
  
(in-package :xpc-system)

(defsystem xpc
  :name "xpc"
  :serial t
  :components ((:file "packages")
               (:file "globals")
               (:file "utils")
               (:file "sysreg")
               (:file "clock")
               (:file "simulator")               
               (:file "scheduler")
               (:module "model"
                        :serial t
                        :components
                        ((:file "graph")
                         (:file "pwr-dist-network")
                         (:file "engine")
                         (:file "flight-params")
                         (:file "misc")
                         (:file "panel-leds")
                         (:file "electrical")
                         (:file "elec-logic")
                         (:file "lights")
                         (:file "flaps")
                         (:file "gear")
                         (:file "aircraft")
                         (:file "observers")))
               (:module "xplane"
                        :serial t
                        :components
                        ((:file "packages")
                         (:file "byte-utils") 
                         (:file "manager")))
               (:module "display"
                        :serial t
                        :components
                        ((:file "packages")
                         (:file "misc")
                         (:file "image-factory")
                         (:file "switch")
                         (:file "panel")
                         (:file "adiru-panel")
                         (:file "air-cond-panel")
                         (:file "anti-ice-panel")
                         (:file "bleed-air-panel")
                         (:file "electrical-panel")
                         (:file "emer-lights-panel")
                         (:file "fuel-panel")
                         (:file "hydraulic-panel")
                         (:file "left-wiper-panel")
                         (:file "pass-signs-panel")
                         (:file "right-wiper-panel")
                         (:file "start-panel")
                         (:file "general")
                         (:file "overhead-panel")))
               (:module "tests"
                        :serial t
                        :components
                        ((:file "all-tests")
                         (:file "test-flaps")
                         (:file "test-gear")
                         (:file "test-lights")
                         (:file "test-apu")
                         (:file "test-graph")
                         (:file "test-electrical"))))
  :depends-on (:cl-ppcre
               :fiveam
               :metatilities
               :cl-containers
               :ieee-floats
               :cells
               :log4cl
               :lispbuilder-sdl
               :lispbuilder-sdl-gfx
               :lispbuilder-sdl-image))


