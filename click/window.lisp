; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass window (widget)
  ((visible :initarg :visible
            :initform t)
   (widgets :initform '())
   (title-bar)
   (tags :initform '())
   (themed :initform (not (null *theme-path*)))))

(defmethod initialize-instance :after ((window window) &key)
  (assert-window-manager-exists)
  (with-slots (left-margin right-margin top-margin bottom-margin
               width title-bar x y) window
    (with-node-images (:window :shadows) (corner-left-top corner-right-bottom)
      (setf left-margin (width corner-left-top)
            right-margin (width corner-right-bottom)
            top-margin (height corner-left-top)
            bottom-margin (height corner-right-bottom)))
    (setf title-bar (make-instance 'title-bar 
                                   :width width
                                   :x-offset x
                                   :y-offset y))
    (add-window *window-manager* window)))

(defmethod draw ((window window))
  (with-slots (title-bar) window
    (draw-shadows window)
    (draw-panel window)
    (draw title-bar)
    (dolist (widget (slot-value window 'widgets))
      (draw widget))))

(defmethod draw-shadows ((window window))
  (with-slots (width height left-margin right-margin
               top-margin bottom-margin) window
    (let ((ax (abs-x window))
          (ay (abs-y window)))
      (with-node-images (:window :shadows)
          (corner-left-top top-left top-centre top-right
           corner-right-top right-top right-centre right-bottom
           corner-right-bottom bottom-right bottom-centre bottom-left
           corner-left-bottom left-bottom left-centre left-top)
        (enable-alpha-textures)
        (draw-at corner-left-top (- ax left-margin) (- ay top-margin))
        (draw-at top-left ax (- ay top-margin))
        (draw-tiled top-centre (+ ax (width top-left)) (- ay top-margin)
                    :width (- width (width top-left) (width top-right)))
        (draw-at top-right (+ ax (- width (width top-right))) (- ay top-margin))
        (draw-at corner-right-top (+ ax width) (- ay top-margin))
        (draw-at right-top (+ ax width) ay)
        (draw-tiled right-centre (+ ax width) (+ ay (height right-top))
                    :height (- height 
                               (height right-top)
                               (height right-bottom)))
        (draw-at right-bottom
                 (+ ax width) (+ ay (- height (height right-bottom))))
        (draw-at corner-right-bottom (+ ax width) (+ ay height))
        (draw-at bottom-right
                 (+ ax (- width (width bottom-right))) (+ ay height))
        (draw-tiled bottom-centre (+ ax (width bottom-left)) (+ ay height)
                    :width (- width (width bottom-left) (width bottom-right)))
        (draw-at bottom-left ax (+ ay height))
        (draw-at corner-left-bottom (- ax left-margin) (+ ay height))
        (draw-at left-bottom
                 (- ax left-margin)
                 (+ ay (- height (height left-bottom))))
        (draw-tiled left-centre (- ax left-margin) (+ ay (height left-top))
                    :height (- height (height left-bottom) (height left-top)))
        (draw-at left-top (- ax left-margin) ay)))))

(defmethod draw-panel ((window window))
  (with-slots (width height title-bar) window
    (let ((ax (abs-x window))
          (ay (abs-y window))
          (title-bar-height (height title-bar)))
      (with-node-images (:window :panel)
          (left corner-left-bottom bottom corner-right-bottom right background)
        (draw-tiled left ax (+ ay title-bar-height)
                    :height (- height title-bar-height
                               (height corner-left-bottom)))
        (draw-at corner-left-bottom
                 ax (+ ay (- height (height corner-left-bottom))))
        (draw-tiled bottom
                    (+ ax (width corner-left-bottom))
                    (+ ay (- height (height bottom)))
                    :width (- width
                              (width corner-left-bottom)
                              (width corner-right-bottom)))
        (draw-at corner-right-bottom
                 (+ ax (- width (width corner-right-bottom)))
                 (+ ay (- height (height corner-right-bottom))))
        (draw-tiled right (+ ax (- width (width right))) (+ ay title-bar-height)
                    :height (- height title-bar-height
                               (height corner-right-bottom)))
        (draw-tiled background
                    (+ ax (width left)) (+ ay title-bar-height)
                    :height (- height title-bar-height (height bottom))
                    :width (- width (width left) (width right)))))))