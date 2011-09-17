; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defclass painter (widget)
  ((sprite :initarg :sprite
           :initform (error "sprite must be specified"))))

(defmethod initialize-instance :after ((widget painter) &key)
  (with-slots (sprite width height) widget
    (when (zerop width) (setf width (width sprite)))
    (when (zerop height) (setf height (height sprite)))))

(defmethod draw ((widget painter))
  (with-slots (sprite width height) widget
    (draw-sprite sprite :width width :height height)))
