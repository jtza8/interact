; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defparameter *sprites* '())

(defclass sprite ()
  ((width :initarg :width
          :initform (error "No width given.")
          :reader width)
   (height :initarg :height
           :initform (error "No height given.")
           :reader height)))

(defmethod initialize-instance :after ((sprite sprite) &key)
  (push sprite *sprites*))

(defmethod clone ((sprite sprite) &rest init-args)
  (declare (ignore init-args))
  sprite)

(defgeneric draw-sprite (sprite &key x y &allow-other-keys)
  (:documentation
   "Draw at the specified coordinates."))

(defmethod free ((sprite sprite))
  ())

(internal rectangle)
(defun rectangle (x y width height &key (tex-coords '(0 0 1 0 1 1 0 1)))
  "Draws the currently bound texture as a 2D \"surface\""
  (let ((coords tex-coords)
        (x (truncate x))
        (y (truncate y)))
    (gl:with-primitive :quads
      (gl:tex-coord (pop coords) (pop coords))
      (gl:vertex x y)
      (gl:tex-coord (pop coords) (pop coords))
      (gl:vertex (+ x width) y)
      (gl:tex-coord (pop coords) (pop coords))
      (gl:vertex (+ x width) (+ y height))
      (gl:tex-coord (pop coords) (pop coords))
      (gl:vertex x (+ y height)))))

(internal delete-all-sprites)
(defun delete-all-sprites ()
  (map nil #'free *sprites*))