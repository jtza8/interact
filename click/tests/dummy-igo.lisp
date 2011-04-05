; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass dummy-igo (igo)
  ((latest-event :initform nil
                 :reader latest-event)))

(defmethod initialize-instance :after ((igo igo) &key)
  (provide-events igo :dummy-event)
  (desire-events igo :mouse-move #'event-mouse-move))

;; (defmethod select-handler ((dummy dummy-igo) event-type)
;;   (when (eq event-type :mouse-move)
;;     #'event-mouse-move))
  
(defmethod event-mouse-move ((dummy dummy-igo) event)
  (setf (slot-value dummy 'latest-event) event))