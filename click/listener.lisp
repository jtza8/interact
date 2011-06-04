; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass listener ()
  ((desired-events :initform '()
                   :reader desired-events)))

(defmethod select-handler ((listener listener) event-type)
  (getf (slot-value listener 'desired-events) event-type))

(defmethod desire-events ((listener listener) &rest desired)
  (assert (evenp (length desired)) () "odd key/value pair ~S" desired)
  (with-slots (desired-events) listener
    (loop for (key value) on desired by #'cddr
          do (setf (getf desired-events key) value))))

(defmethod undesire-events ((listener listener) &rest event-keys)
  (with-slots (desired-events) listener
    (setf desired-events
          (loop for (key value) on desired-events by #'cddr
                when (not (find key event-keys))
                collect key and collect value))))
