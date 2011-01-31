; Copyright 2011 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass simple-widget (widget)
  ())

(defmethod draw ((widget simple-widget))
  (with-slots (x y) widget
    (with-sprites (target) (sprite-node)
      (draw-at target x y))))