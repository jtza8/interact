; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass widget (listenable listener)
  ((x :initform 0
      :initarg :x)
   (y :initform 0
      :initarg :y)
   (x-offset :initform 0
             :initarg :x-offset
             :accessor x-offset)
   (y-offset :initform 0
             :initarg :y-offset
             :accessor y-offset)
   (left-margin :initform 0
		:initarg :left-margin)
   (right-margin :initform 0
		 :initarg :right-margin)
   (top-margin :initform 0
              :initarg :top-margin)
   (bottom-margin :initform 0
		  :initarg :bottom-margin)
   (width :initform 50
          :initarg :width
          :reader width)
   (height :initform 20
           :initarg :height
           :reader height)))

(defmethod abs-x ((widget widget))
  "Returns the absolute x-coordinate of the widget."
  (with-slots (x x-offset) widget
    (+ x x-offset)))

(defmethod abs-y ((widget widget))
  "Returns the absolute y-coordinate of the widget."
  (with-slots (y y-offset) widget
    (+ y y-offset)))

(defmethod within ((widget widget) x y)
  "Determains whether the coordinate specified is within the widget."
  (with-slots (x-offset y-offset width height) widget
    (let ((ax (abs-x widget))
          (ay (abs-y widget)))
      (and (< ax x (+ ax width))
           (< ay y (+ ay height))))))