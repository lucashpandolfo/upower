(in-package #:upower)

(defun enumerate-devices (bus)
  (dbus:invoke-method (dbus:bus-connection bus)
		      "EnumerateDevices"
		      :path "/org/freedesktop/UPower"
		      :interface "org.freedesktop.UPower"
		      :destination "org.freedesktop.UPower"
		      :signature ()))

(defun device-type (bus device)
"Type of power source."
  (case (dbus:get-property bus device "org.freedesktop.UPower" "Type")
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
  (case (dbus:get-property bus device "org.freedesktop.UPower" "State")
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
  (dbus:get-property bus device "org.freedesktop.UPower" "Percentage"))

(defun device-time-to-full (bus device)
"Number of seconds until the power source is considered full. Is set
to 0 if unknown."
  (dbus:get-property bus device "org.freedesktop.UPower" "TimeToFull"))

(defun device-time-to-empty (bus device)
"Number of seconds until the power source is considered empty. Is set
to 0 if unknown."
  (dbus:get-property bus device "org.freedesktop.UPower" "TimeToEmpty"))

(defun device-online (bus device)
  "Whether power is currently being provided through line power. This
property is only valid if the property type has the value :line-power"
  (dbus:get-property bus device "org.freedesktop.UPower" "Online"))

(defun device-power-supply (bus device)
"If the power device is used to supply the system. This would be set
TRUE for laptop batteries and UPS devices, but set FALSE for wireless
mice or PDAs."
  (dbus:get-property bus device "org.freedesktop.UPower" "PowerSupply"))
