(defpackage #:upower
  (:use #:cl #:dbus)
  (:export 
   #:main-loop
   #:get-devices-info))