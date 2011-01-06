; Copyright 2010 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass label (widget)
  ((desired-events :initform '(:window-move event-window-move))
   (text :initarg :text
         :initform (error "no text specified")
         :reader text)
   sprite))

(defmethod initialize-instance :after ((label label) &key)
  (with-slots (sprite text) label
    (setf sprite (render-text text))))

(defmethod draw ((label label))
  (with-slots (x y sprite) label
    (let ((ax (abs-x label))
          (ay (abs-y label)))
      (draw-at sprite ax ay))))

(defmethod event-window-move ((label label) event)
  (with-slots (x-offset y-offset) label
    (with-event-keys (x y) event
      (setf x-offset x
            y-offset y))))