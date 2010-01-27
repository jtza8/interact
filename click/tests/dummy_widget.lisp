; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass dummy-widget (widget)
  ((listenable-events :initform '(:dummy-event))
   (listen-for-events :initform '(:mouse-move))
   (latest-event :initform nil
                 :reader latest-event)))

(defmethod select-handler ((dummy dummy-widget) event-type)
  (when (eq event-type :mouse-move)
    #'event-mouse-move))
  
(defmethod event-mouse-move ((dummy dummy-widget) event)
  (setf (slot-value dummy 'latest-event) event))