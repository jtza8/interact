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
           :reader height)))

(defmethod draw-at ((image image) x y)
  (with-slots (texture width height) image
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:enable :texture-2d)
    (gl:bind-texture :texture-2d texture)
    (gl:with-primitive :quads
      (gl:tex-coord 0 0)
      (gl:vertex x y)
      (gl:tex-coord 1 0)
      (gl:vertex (+ x width) y)
      (gl:tex-coord 1 1)
      (gl:vertex (+ x width) (+ y height))
      (gl:tex-coord 0 1)
      (gl:vertex x (+ y height)))))

(defmethod move ((image image) new-x new-y)
  (with-slots (x y) image
    (setf x new-x 
          y new-y)))