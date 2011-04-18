; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass interactive-container (container)
  ((timer :initform (make-instance 'stopwatch))
   (button :initform nil)))

(defmethod initialize-instance :after ((container interactive-container) &key)
  (desire-events container :key-down #'keyboard-event :key-up #'keyboard-event))

(defmethod draw :before ((container interactive-container))
  (with-slots (rotation button timer) container
    (case button
      (:left (incf rotation (/ (lap timer) 10))
             (reset timer :auto-start t))
      (:right (decf rotation (/ (lap timer) 10))
              (reset timer :auto-start t)))))

(defmethod keyboard-event ((container interactive-container) event)
  (with-slots (button timer) container
    (case (event-type event)
      (:key-down (with-event-keys (key) event
                   (when (find key '(:left :right))
                     (start timer)
                     (setf button key))))
      (:key-up (with-event-keys (key) event
                 (when (find key '(:left :right))
                   (reset timer)
                   (setf button nil))))))
  (send-event container event))