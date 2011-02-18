; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass color-sprite (sprite)
  ((color :initarg :color
          :initform (error "No color specified.")
          :reader color)))

(defmethod draw-tiled ((sprite color-sprite) x y &key (width 1) (height 1))
  (with-slots (color) sprite
    (gl:with-primitive :quads
      (apply #'gl:color color)
      (gl:vertex x y)
      (gl:vertex (+ x width) y)
      (gl:vertex (+ x width) (+ y height))
      (gl:vertex x (+ y height)))))

(defmethod draw-at ((sprite color-sprite) x y)
  (draw-tiled sprite x y))