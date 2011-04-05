; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass simple-igo (igo)
  ((sprite :initarg :sprite
           :initform (error "sprite must be specified"))))

(defmethod draw ((igo simple-igo))
  (with-slots (sprite x y) igo
    (draw-at sprite x y)))