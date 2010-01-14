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

(defmethod draw-at ((image image) x y)
  (with-slots (texture width height) image
    (gl:bind-texture :texture-2d texture)
    (rectangle x y width height)))

(defmethod draw-tiled ((image image) x y &optional custom-width custom-height)
  (with-slots (width height texture) image
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-wrap-s t)
    (gl:tex-parameter :texture-2d :texture-wrap-t t)
    (when (null custom-width) (setf custom-width width))
    (when (null custom-height) (setf custom-height height))
    (rectangle x y custom-width custom-height
               :tex-coords (list 0 0
                                 (/ custom-width width) 0
                                 (/ custom-width width) (/ custom-height height)
                                 0 (/ custom-height height)))))