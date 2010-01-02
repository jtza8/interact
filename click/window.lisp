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
    (gl:color 1 1 1)
    (let* ((shadow (fetch-texture *theme-texture-tree* :window :shadow))
           (corner-tl (fetch-texture shadow :corner-top-left))
           (corner-tr (fetch-texture shadow :corner-top-right)))
      (draw-texture shadow
                   
          
                    
                  
