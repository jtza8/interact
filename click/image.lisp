; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass image ()
  ((texture :initarg :texture
            :initform (error "No texture name given.")
            :reader texture)
   (width :initarg :width
          :initform (error "No width given.")
          :reader width)
   (height :initarg :height
           :initform (error "No height given.")
           :reader height)
   (x :initarg :x
      :initform 0
      :accessor x)
   (y :initarg :y
      :initform 0
      :accessor y)))


(defmethod draw ((image image))
  (with-slots (texture width height x y) image
    (gl:bind-texture :texture-2d texture)
    (gl:with-primitive :quads
      (gl:vertex x y)
      (gl:vertex (+ x width) y)
      (gl:vertex (+ x width) (+ y height))
      (gl:vertex x (+ y height)))))

(defmethod move-to ((image image) new-x new-y)
  (with-slots (x y) image
    (setf x new-x 
          y new-y)))