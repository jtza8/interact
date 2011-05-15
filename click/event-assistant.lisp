; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass event-assistant (listener)
  ((quit-key :initarg :quit-key
             :initform nil)
   (fullscreen-key :initarg :fullscreen-key
                   :initform nil)))

(define-instance-maker event-assistant)

(defmethod initialize-instance :after ((assistant event-assistant) &key)
  (with-slots (quit-key fullscreen-key) assistant
    (when (notevery #'null (list quit-key fullscreen-key))
      (desire-events assistant :key-down #'key-handler))))

(internal key-handler)
(defmethod key-handler ((assistant event-assistant) event)
  (with-slots (quit-key fullscreen-key) assistant
    (with-event-keys (key) event
      (cond ((eq key quit-key) (sdl:push-quit-event))
            ((eq key fullscreen-key) (toggle-fullscreen))))))
