; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass sprite ()
  ((width :initarg :width
          :initform (error 'program-error "No width given.")
          :reader width)
   (height :initarg :height
           :initform (error 'program-error "No height given.")
           :reader height)))

(defun rectangle (x y width height &key (tex-coords '(0 0 1 0 1 1 0 1)))
  "Draws the currently bound texture as a 2D \"surface\""
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

(defgeneric draw-at (sprite x y)
  (:documentation
   "Draw at the specified coordinates."))

(defgeneric draw-tiled (sprite x y &key width height)
  (:documentation
   "Draw Tiled."))
