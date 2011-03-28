; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass dummy-widget (widget)
  ((latest-event :initform nil
                 :reader latest-event)))

(defmethod initialize-instance :after ((widget widget) &key)
  (provide-events widget :dummy-event)
  (desire-events widget :mouse-move #'event-mouse-move))

;; (defmethod select-handler ((dummy dummy-widget) event-type)
;;   (when (eq event-type :mouse-move)
;;     #'event-mouse-move))
  
(defmethod event-mouse-move ((dummy dummy-widget) event)
  (setf (slot-value dummy 'latest-event) event))