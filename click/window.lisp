; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass window (widget)
  ((title :initarg :title
          :initform "Untitled")
   (visible :initarg :visible
            :initform t)
   (widgets :initform '())
   (tags :initform '())
   (themed :initform (not (null *theme-path*)))))

(defmethod initialize-instance :after ((window window) &key)
  (assert-window-manager-exists)
  (with-slots (left-margin right-margin top-margin bottom-margin) window
    (with-node-images (:window :shadow) (corner-left-top corner-right-bottom)
      (setf left-margin (width corner-left-top)
            right-margin (width corner-right-bottom)
            top-margin (height corner-left-top)
            bottom-margin (height corner-right-bottom))))
  (add-window *window-manager* window))

(defmethod draw ((window window))
  (with-slots (x y width height left-margin right-margin
               top-margin bottom-margin) window
    (with-node-images (:window :shadow)
        (corner-left-top top-01 top-02)
      (enable-alpha-textures)
      (draw-at corner-left-top (- x left-margin) (- y top-margin))
      (draw-at top-01 x (- y top-margin))
      (draw-tiled top-02 (+ x (width top-01)) (- y top-margin)
                  :width (- width (width top-01))))))