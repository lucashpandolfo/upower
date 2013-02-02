;; Battery module for STUMPWM.
;; upower (and dbus) lisp bindings needs to be loaded before this module

(defpackage #:stumpwm-upower
  (:use #:cl)
  (:export #:upower-helper))

(in-package #:stumpwm-upower)

(pushnew '(#\b fmt-battery) stumpwm:*screen-mode-line-formatters* :test 'equal)

(defparameter *all-devices* nil)

(defparameter *devices-info* nil
  "A list of lists containing 
 (battery-name charge% state time-to-full time-to-empty)")

(defun format-battery-state (data)
  (let ((state (nth 2 data))
        (percentage (round (nth 1 data)))
        color (symbol "^B^[^3*?^]"))
    (cond
      ((> percentage 80) (setf color "^B^[^2*"))
      ((> percentage 50) (setf color "^b^[^3*"))
      ((> percentage 20) (setf color "^B^[^3*"))
      ((> percentage 10) (setf color "^b^[^1*"))
      (t (setf color "^B^[^1*")))
    (case  state
      (:charging (setf symbol "^B^[^2*+^]"))
      (:discharging (setf symbol "^B^[^1*-^]"))
      (:fully-charged (setf symbol "^b^[^3*=^]")))
    (if (< percentage 100)
         (format nil "~a~a~2d%^]^b" symbol color percentage)
         (format nil "~a~2d%^]^b" color percentage))))

(defun main-loop ()
  (dbus:with-open-bus (bus (dbus:system-server-addresses))
    (let ((batteries nil)
	  (message nil))
      (flet ((update-devices ()
	       (setf *all-devices* (upower:enumerate-devices bus))
	       (setf *devices-info* nil)
	       (loop for i in *all-devices*
		  do (when (eq (upower:device-type bus i) :battery)
		       (push i batteries))))
	     (update-state ()
	       (setf *devices-info*
		     (loop for battery in batteries
			collect (list battery 
				      (upower:device-percentage bus battery) 
				      (upower:device-state bus battery)
				      (upower:device-time-to-full bus battery)
				      (upower:device-time-to-empty bus battery))))))

	(update-devices)
	(update-state)
	(dbus:add-match bus :path "/org/freedesktop/UPower" :interface "org.freedesktop.UPower" :member "DeviceChanged")
	(dbus:add-match bus :path "/org/freedesktop/UPower" :interface "org.freedesktop.UPower" :member "DeviceAdded")
	(dbus:add-match bus :path "/org/freedesktop/UPower" :interface "org.freedesktop.UPower" :member "DeviceRemoved")
	(loop do
	     (setf message (dbus::wait-for-incoming-message (dbus:bus-connection bus) '(dbus:signal-message)))
	     (when (typep message 'dbus:signal-message)
	       (cond
		 ((string-equal (dbus:message-member message) "DeviceChanged") (update-state))
		 ((string-equal (dbus:message-member message) "DeviceAdded") (update-devices))
		 ((string-equal (dbus:message-member message) "DeviceRemoved") (update-devices))
		 (t nil))))))))

(defun get-devices-info ()
  *devices-info*)

(defun fmt-battery (ml)
  (declare (ignore ml))
  (let ((states (get-devices-info)))
    (if (null states)
        "^B^[^4*LINE^]^b"
        (format-battery-state (car states)))))
    
(defun upower-helper ()
  "Starts a new thread for monitoring changes in upower."
  (bordeaux-threads:make-thread #'main-loop :name "Upower: main loop thread"))
