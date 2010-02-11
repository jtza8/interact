; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass sprite ()
  ((width :initarg :width
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

(defgeneric draw (sprite)
  (:documentation "When called, a sprite should draw itself using OpenGL."))

(defgeneric draw-at (sprite x y)
  (:documentation
   "Exactly like draw, except that the x and y coordinates are
specified manually"))
