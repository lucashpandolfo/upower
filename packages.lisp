(defpackage #:upower
  (:use #:cl #:dbus)
  (:export 
   #:enumerate-devices
   #:device-type
   #:device-state
   #:device-percentage
   #:device-time-to-full
   #:device-time-to-empty
   #:device-online
   #:device-power-supply))
