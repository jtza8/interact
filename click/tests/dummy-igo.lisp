; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass dummy-igo (igo)
  ((latest-event :initform nil
                 :reader latest-event)))

(defmethod initialize-instance :after ((igo dummy-igo) &key
                                       (provided-events '(:dummy-event))
                                       (desired-events '(:mouse-motion)))
  (apply #'provide-events igo provided-events)
  (apply #'desire-events igo 
         (loop for event in desired-events
               collect event
               collect #'event-handler)))

(defmethod event-handler ((dummy dummy-igo) event)
  (setf (slot-value dummy 'latest-event) event))
