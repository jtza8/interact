; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass simple-widget (widget)
  ((sprite :initarg :sprite
           :initform (error 'program-error "sprite must be specified"))))

(defmethod draw ((widget simple-widget))
  (with-slots (sprite x y) widget
    (draw-at sprite x y)))