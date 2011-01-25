; Copyright 2011 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click-gui)

(defclass button (widget)
  ())

(defmethod initialize-instance :after ((button button) &key)
  (desire-events button
                 :mouse-down #'event-mouse-down
                 :mouse-up #'event-mouse-up))

(defmethod draw ((button button))
  (with-slots (x y width height) button
    (with-sprites (:button :up) (left middle right)
      (draw-at left x y)
      (draw-tiled middle (+ x (width left)) y
                        :width (- width (width left) (width right)))
      (draw-at right (+ x (- width (width right))) y))))

(defmethod event-mouse-down ((button button) event) ())
(defmethod event-mouse-up ((button button) event) ())
