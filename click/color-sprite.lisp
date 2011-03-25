; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass color-sprite (sprite)
  ((color :initarg :color
          :initform (error "No color specified.")
          :reader color)
   (width :initarg :width
          :initform 0)
   (height :initarg :height
           :initform 0)))

(defmethod draw-at ((sprite color-sprite) x y &key width height mode)
  (declare (ignore mode))
  (with-slots (color (normal-width width) (normal-height height)) sprite
    (when (null width)
      (setf width normal-width))
    (when (null height)
      (setf height normal-height))
    (gl:with-primitive :quads
      (apply #'gl:color color)
      (gl:vertex x y)
      (gl:vertex (+ x width) y)
      (gl:vertex (+ x width) (+ y height))
      (gl:vertex x (+ y height)))))