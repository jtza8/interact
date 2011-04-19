; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass event-assistant (listener)
  ((quit-button :initarg :quit-button)))

(defmethod initialize-instance :after ((assistant event-assistant) &key)
  (with-slots (quit-button) assistant
    (unless (null quit-button)
      (desire-events assistant :key-down #'key-handler))))

(defmethod key-handler ((assistant event-assistant) event)
  (with-slots (quit-button) assistant
    (with-event-keys (key) event
      (when (eq key quit-button)
        (sdl:push-quit-event)))))