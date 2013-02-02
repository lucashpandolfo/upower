upower: UPower bindings for common lisp
=======================================

A module for use with STUMPWM is provided.

Module setup
------------

Copy or symlink the stumpwm/upower-manager.lisp over to your stumpwm
plugins directory and add the following code to your .stumpwmrc file

``` Lisp
;;Load the module and dependences
(ql:quickload "upower")
(load-module "upower-manager")

;;Handy commands
(stumpwm:defcommand launch-upower-manager () ()
  "Launch (or relaunch) upower manager"
  (stumpwm-upower:upower-manager-launch))

(stumpwm:defcommand suspend () ()
  "Suspend via UPower"
  (stumpwm-upower:suspend))

(stumpwm:defcommand hibernate () ()
  "Hibernate via UPower"
  (stumpwm-upower:hibernate))

;; Launch at init
(launch-upower-manager)
```

Provides a battery formatter (%b).