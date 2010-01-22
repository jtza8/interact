; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass title-bar (widget)
  ((title :initarg :title
          :initform "Untitled"
          :accessor title)))

(defmethod initialize-instance :after ((title-bar title-bar) &key)
  (with-node-images (:window :title-bar) (left centre right)
    (setf (slot-value title-bar 'height)
          (apply #'max (mapcar #'height (list left centre right))))))

(defmethod draw ((title-bar title-bar))
  (with-slots (width height) title-bar
    (let ((ax (abs-x title-bar))
          (ay (abs-y title-bar)))
      (with-node-images (:window :title-bar) (left centre right)
        (draw-at left ax ay)
        (draw-tiled centre (+ ax (width left)) ay
                    :width (- width (width left) (width right)))
        (draw-at right (+ ax (- width (width right))) ay)))))