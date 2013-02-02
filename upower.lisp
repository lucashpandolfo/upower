(in-package #:upower)

(defparameter *upower-service* "org.freedesktop.UPower")

(defun enumerate-devices (bus)
  (dbus:invoke-method (dbus:bus-connection bus)
		      "EnumerateDevices"
		      :path "/org/freedesktop/UPower"
		      :interface "org.freedesktop.UPower"
		      :destination *upower-service*
		      :signature ()))

(defun device-type (bus device)
  "Type of power source."
  (case (dbus:get-property bus *upower-service* device "org.freedesktop.UPower" "Type")
    (0 :unknown)
    (1 :line-power)
    (2 :battery)
    (3 :ups)
    (4 :monitor)
    (5 :mouse)
    (6 :keyboard)
    (7 :pda)
    (8 :phone)))

(defun device-state (bus device)
"The battery power state."
  (case (dbus:get-property bus *upower-service* device "org.freedesktop.UPower" "State")
    (0 :unknown)
    (1 :charging)
    (2 :discharging)
    (3 :empty)
    (4 :fully-charged)
    (5 :pending-charge)
    (6 :pending-discharge)))

(defun device-percentage (bus device)
"The amount of energy left in the power source expressed as a
percentage between 0 and 100. "
  (dbus:get-property bus *upower-service* device "org.freedesktop.UPower" "Percentage"))

(defun device-time-to-full (bus device)
"Number of seconds until the power source is considered full. Is set
to 0 if unknown."
  (dbus:get-property bus *upower-service* device "org.freedesktop.UPower" "TimeToFull"))

(defun device-time-to-empty (bus device)
"Number of seconds until the power source is considered empty. Is set
to 0 if unknown."
  (dbus:get-property bus *upower-service* device "org.freedesktop.UPower" "TimeToEmpty"))

(defun device-online (bus device)
  "Whether power is currently being provided through line power. This
property is only valid if the property type has the value :line-power"
  (dbus:get-property bus *upower-service* device "org.freedesktop.UPower" "Online"))

(defun device-power-supply (bus device)
"If the power device is used to supply the system. This would be set
TRUE for laptop batteries and UPS devices, but set FALSE for wireless
mice or PDAs."
  (dbus:get-property bus *upower-service* device "org.freedesktop.UPower" "PowerSupply"))

(defun suspend (bus)
  "Suspends the computer into a low power state. System state is not
preserved if the power is lost."
  (dbus:invoke-method (dbus:bus-connection bus)
		      "Suspend"
		      :path "/org/freedesktop/UPower"
		      :interface "org.freedesktop.UPower"
		      :destination *upower-service*
		      :signature ()))

(defun hibernate (bus)
  "Hibernates the computer into a low power state. System state is
preserved if the power is lost."
  (dbus:invoke-method (dbus:bus-connection bus)
		      "Hibernate"
		      :path "/org/freedesktop/UPower"
		      :interface "org.freedesktop.UPower"
		      :destination *upower-service*
		      :signature ()))

(defun device-history (bus device-path type timespan resolution)
  (dbus:invoke-method (dbus:bus-connection bus)
		      "GetHistory"
		      :path "/org/freedesktop/UPower/devices/battery_C23B"
		      :interface "org.freedesktop.UPower.Device"
		      :destination *upower-service*
		      :arguments (list type timespan resolution)
		      :signature "suu"))

(defun device-history (bus device-path type timespan resolution)
"Gets history for the power device that is persistent across reboots.

Type: The type of history. Valid types are :rate or :charge.

Timespan: The amount of data to return in seconds, or -1 for all.

Resolution: The approximate number of points to return. A higher
resolution is more accurate, at the expense of plotting speed."
  (dbus:invoke-method (dbus:bus-connection bus)
		      "GetHistory"
		      :path device-path
		      :interface "org.freedesktop.UPower.Device"
		      :destination *upower-service*
		      :arguments (list (format nil "~(~a~)" type) timespan resolution)
		      :signature "suu"))

(defun device-statistics (bus device-path type)
  "Gets statistics for the power device that may be interesting to
show on a graph in the session.  Type can be :charging
or :discharging."
  (dbus:invoke-method (dbus:bus-connection bus)
		      "GetStatistics"
		      :path device-path
		      :interface "org.freedesktop.UPower.Device"
		      :destination *upower-service*
		      :arguments (list (format nil "~(~a~)" type))
		      :signature "s"))



  

