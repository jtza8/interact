; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defclass dummy-widget (widget)
  ((latest-event :initform nil
                 :reader latest-event)))

(defmethod initialize-instance :after ((widget dummy-widget) &key
                                       (provided-events '(:dummy-event))
                                       (desired-events '(:mouse-motion)))
  (apply #'provide-events widget provided-events)
  (apply #'desire-events widget 
         (loop for event in desired-events
               collect event
               collect #'event-handler)))

(defmethod event-handler ((dummy dummy-widget) event)
  (setf (slot-value dummy 'latest-event) event))
