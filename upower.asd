(asdf:defsystem #:upower
  :depends-on (#:dbus)
  :serial t
  :components
  ((:file "packages")
   (:file "upower")))