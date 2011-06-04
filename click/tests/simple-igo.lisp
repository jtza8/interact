; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass simple-igo (igo)
  ((sprite :initarg :sprite
           :initform (error "sprite must be specified"))))

(define-instance-maker simple-igo)

(defmethod initialize-instance :after ((igo simple-igo) &key)
  (with-slots (sprite width height) igo
    (setf width (width sprite)
          height (height sprite))))

(defmethod draw ((igo simple-igo))
  (with-slots (sprite) igo
    (draw-sprite sprite)))
