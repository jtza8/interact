; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass dummy-listener (listener)
  ((latest-event :initform nil
                 :reader latest-event)
   (listening-request-called :initform nil
                             :reader listening-request-called-p)
   (listener-removal-notice-called :initform nil
                                   :reader listener-removal-notice-called-p)))

(defmethod initialize-instance :after ((listener dummy-listener) &key 
                                       (desired-events '(:dummy-event)))
  (apply #'desire-events listener
         (loop for event in desired-events
               collect event
               collect #'event-handler)))

(defmethod listening-request :before ((listener dummy-listener)
                                      (listenable listenable)
                                      event-type)
  (setf (slot-value listener 'listening-request-called) t))

(defmethod listener-removal-notice :before ((listener dummy-listener)
                                            (listenable listenable)
                                            event-type)
  (setf (slot-value listener 'listener-removal-notice-called) t))

(defmethod event-handler ((listener dummy-listener) event)
  (setf (slot-value listener 'latest-event) event))
