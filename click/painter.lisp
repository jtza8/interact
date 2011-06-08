; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass painter (igo)
  ((sprite :initarg :sprite
           :initform (error "sprite must be specified"))))

(define-instance-maker painter)

(defmethod initialize-instance :after ((igo painter) &key)
  (with-slots (sprite width height) igo
    (when (zerop width) (setf width (width sprite)))
    (when (zerop height) (setf height (height sprite)))))

(defmethod draw ((igo painter))
  (with-slots (sprite width height) igo
    (draw-sprite sprite :width width :height height)))
