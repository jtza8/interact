; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass dummy-listener (listener)
  ((latest-event :initform nil
                 :reader latest-event)))

(defmethod initialize-instance :after ((listener dummy-listener) &key 
                                       (desired-events '(:dummy-event)))
  (apply #'desire-events listener
         (loop for event in desired-events
               collect event
               collect #'event-handler)))

(defmethod event-handler ((listener dummy-listener) event)
  (setf (slot-value listener 'latest-event) event))
