; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass dummy-widget (widget)
  ((listenable-events :initform '(:dummy-event))
   (subscribe-events :initform '(:mouse-move))
   (latest-event :initform nil
                 :reader latest-event)))

(defmethod get-event-handler ((widget dummy-widget) event)
  (when (eq (event-type event) :dummy-event)
    #'event-dummy))
  
(defmethod event-mouse-move ((dummy dummy-widget) event)
  (setf (slot-value dummy 'latest-event) event))