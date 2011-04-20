; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass event-assistant (listener)
  ((quit-key :initarg :quit-key)))

(defmethod initialize-instance :after ((assistant event-assistant) &key)
  (with-slots (quit-key) assistant
    (unless (null quit-key)
      (desire-events assistant :key-down #'key-handler))))

(defmethod key-handler ((assistant event-assistant) event)
  (with-slots (quit-key) assistant
    (with-event-keys (key) event
      (when (eq key quit-key)
        (sdl:push-quit-event)))))