; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass interactive-container (container)
  ((buttons :initform nil)))

(defmethod initialize-instance :after ((container interactive-container) &key)
  (desire-events container :key-down #'keyboard-event :key-up #'keyboard-event))

(defmethod draw :before ((container interactive-container))
  (with-slots (rotation buttons) container
    (case (car buttons)
      (:left (incf rotation 2))
      (:right (decf rotation 2)))))

(defmethod keyboard-event ((container interactive-container) event)
  (with-slots (buttons timer) container
    (case (event-type event)
      (:key-down (with-event-keys (key) event
                   (when (find key '(:left :right))
                     (push key buttons))))
      (:key-up (with-event-keys (key) event
                 (when (find key '(:left :right))
                   (setf buttons (remove key buttons)))))))
  (send-event container event))
