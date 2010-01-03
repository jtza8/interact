; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass window (widget)
  ((title :initarg :title
          :initform "Untitled")
   (visible :initarg :visible
            :initform nil)
   (widgets :initform '())
   (tags :initform '())
   (x :initform 10
      :initarg :x)
   (y :initform 10
      :initarg :y)
   (width :initform 100
          :initarg :width)
   (height :initform 100
           :initarg :height)
   (themed :initform (not (null *theme-path*)))
   (textures :initform '())))

(defmethod initialize-instance :after ((window window) &key)
  (assert-window-manager-exists)
  (add-window *window-manager* window))

;(defmethod load-textures ((window window))
;  (with-slots (themed textures) window
;    (unless themed (return-from load-textures))
;    (

(defmethod draw ((window window))
  (with-slots (x y width height) window
    (with-node-images (corner-left-top top-01 corner-right-top)
        (fetch-image-node :window :shadow)
      (draw-at corner-left-top
               (- x (width corner-left-top))
               (- y (height corner-left-top)))
      (draw-at top-01 x (- y (height top-01)))
      (draw-at corner-right-top (+ x width) (- y (height corner-right-top))))))