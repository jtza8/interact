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
        (corner-left-top top-left top-centre top-right
         corner-right-top right-top right-centre right-bottom
         corner-right-bottom bottom-right bottom-centre bottom-left
         corner-left-bottom left-bottom left-centre left-top)
      (enable-alpha-textures)
      (draw-at corner-left-top (- x left-margin) (- y top-margin))
      (draw-at top-left x (- y top-margin))
      (draw-tiled top-centre (+ x (width top-left)) (- y top-margin)
                  :width (- width (width top-left) (width top-right)))
      (draw-at top-right (+ x (- width (width top-right))) (- y top-margin))
      (draw-at corner-right-top (+ x width) (- y top-margin))
      (draw-at right-top (+ x width) y)
      (draw-tiled right-centre (+ x width) (+ y (height right-top))
                  :height (- height 
                             (height right-top)
                             (height right-bottom)))
      (draw-at right-bottom (+ x width) (+ y (- height (height right-bottom))))
      (draw-at corner-right-bottom (+ x width) (+ y height))
      (draw-at bottom-right (+ x (- width (width bottom-right))) (+ y height))
      (draw-tiled bottom-centre (+ x (width bottom-left)) (+ y height)
                  :width (- width (width bottom-left) (width bottom-right)))
      (draw-at bottom-left x (+ y height))
      (draw-at corner-left-bottom (- x left-margin) (+ y height))
      (draw-at left-bottom 
               (- x left-margin)
               (+ y (- height (height left-bottom))))
      (draw-tiled left-centre (- x left-margin) (+ y (height left-top))
                  :height (- height (height left-bottom) (height left-top)))
      (draw-at left-top (- x left-margin) y))))