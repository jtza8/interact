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

(defun rectangle (x y width height &key
                  (tex-coords '(0 0 1 0 1 1 0 1)))
  (let ((coords tex-coords))
    (gl:with-primitive :quads
      (gl:tex-coord (pop coords) (pop coords))
      (gl:vertex x y)
      (gl:tex-coord (pop coords) (pop coords))
      (gl:vertex (+ x width) y)
      (gl:tex-coord (pop coords) (pop coords))
      (gl:vertex (+ x width) (+ y height))
      (gl:tex-coord (pop coords) (pop coords))
      (gl:vertex x (+ y height)))))

(defun enable-alpha-textures ()
  (gl:enable :blend)
  (gl:enable :texture-2d)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear :color-buffer-bit))


(defmethod draw-at ((image image) x y)
  (with-slots (texture width height) image
    (gl:bind-texture :texture-2d texture)
    (rectangle x y width height)))

(defmethod draw-tiled ((image image) x y &key width height)
  (with-slots ((normal-width width) (normal-height height) texture) image
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (when (null width) (setf width normal-width))
    (when (null height) (setf height normal-height))
    (rectangle x y width height
               :tex-coords (list 0 0
                                 (/ width normal-width) 0
                                 (/ width normal-width) (/ height normal-height)
                                 0 (/ height normal-height)))))