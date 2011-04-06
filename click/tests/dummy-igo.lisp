; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass dummy-igo (igo)
  ((latest-event :initform nil
                 :reader latest-event)))

(defmethod initialize-instance :after ((igo dummy-igo) &key)
  (provide-events igo :dummy-event)
  (desire-events igo :mouse-motion #'event-mouse-motion))

(defmethod event-mouse-motion ((dummy dummy-igo) event)
  (setf (slot-value dummy 'latest-event) event))