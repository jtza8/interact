; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass button (widget)
  ((desired-events :initform '(:mouse-down event-mouse-down
                               :mouse-up event-mouse-up
                               :window-move event-window-move))))

(defmethod initialize-instance :after ((button button) &key)
  ())

(defmethod draw ((button button))
  (with-slots (x y width height) button
    (with-node-sprites (:button :up) (left middle right)
      (let ((ax (abs-x button))
            (ay (abs-y button)))
        (draw-at left ax ay)
        (draw-tiled middle (+ ax (width left)) ay
                    :width (- width (width left) (width right)))
        (draw-at right (+ ax (- width (width right))) ay)))))

(defmethod event-mouse-down ((button button) event) ())
(defmethod event-mouse-up ((button button) event) ())

(defmethod event-window-move ((button button) event)
  (with-slots (x-offset y-offset) button
    (with-event-keys (x y) event
      (setf x-offset x
            y-offset y))))