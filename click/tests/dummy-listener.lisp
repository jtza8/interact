; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass dummy-listener (listener)
  ((latest-event :initform nil
                 :reader latest-event)
   (desired-events :initform '(:dummy-event event-dummy))))

(defmethod event-dummy ((listener dummy-listener) event)
  (setf (slot-value listener 'latest-event) event))
