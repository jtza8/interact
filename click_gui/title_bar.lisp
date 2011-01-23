; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass title-bar (widget)
  ((title :initarg :title
          :initform "Untitled"
          :accessor title)
   (desired-events :initform '(:mouse-move event-mouse-move
                               :mouse-down event-mouse-down
                               :mouse-up event-mouse-up))
   (provided-events :initform '(:title-bar-drag))
   (dragging :initform nil
             :accessor dragging)
   (x-drag-offset :initform 0)
   (y-drag-offset :initform 0)))

(defmethod initialize-instance :after ((title-bar title-bar) &key)
  (with-sprites (:window :title-bar) (left centre right)
    (setf (slot-value title-bar 'height)
          (apply #'max (mapcar #'height (list left centre right))))))

(defmethod draw ((title-bar title-bar))
  (with-slots (width height title) title-bar
    (let ((ax (abs-x title-bar))
          (ay (abs-y title-bar)))
      (with-sprites (:window :title-bar) (left centre right)
        (draw-at left ax ay)
        (draw-tiled centre (+ ax (width left)) ay
                    :width (- width (width left) (width right)))
        (draw-at right (+ ax (- width (width right))) ay)))))

(defmethod event-mouse-move ((title-bar title-bar) event)
  (with-slots (x-offset y-offset x-drag-offset y-drag-offset dragging) title-bar
    (unless dragging (return-from event-mouse-move))
    (with-event-keys (x y) event
      (setf x-offset (- x x-drag-offset)
            y-offset (- y y-drag-offset))
      (send-event title-bar
                  (list :title-bar-drag
                        :x-offset x-offset
                        :y-offset y-offset)))))

(defmethod event-mouse-down ((title-bar title-bar) event)
  (with-slots (x-offset y-offset x-drag-offset y-drag-offset dragging) title-bar
    (with-event-keys (x y) event
      (when (within title-bar x y)
        (setf x-drag-offset (- x x-offset)
              y-drag-offset (- y y-offset)
              dragging t)))))

(defmethod event-mouse-up ((title-bar title-bar) event)
  (with-slots (dragging) title-bar
    (when dragging (setf dragging nil))))