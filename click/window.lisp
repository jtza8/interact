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
   (themed :initform (not (null *theme-path*)))))

(defmethod initialize-instance :after ((window window) &key)
  (assert-window-manager-exists)
  (make-sprite window)
  (add-window *window-manager* window))

(defmethod draw-at ((window window) x y)
  (with-slots (texture width height) window
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:enable :texture-2d)
    (gl:bind-texture :texture-2d texture)
    (gl:with-primitive :quads
      (gl:tex-coord 0 0)
      (gl:vertex x y)
      (gl:tex-coord 1 0)
      (gl:vertex (+ x width) y)
      (gl:tex-coord 1 1)
      (gl:vertex (+ x width) (+ y height))
      (gl:tex-coord 0 1)
      (gl:vertex x (+ y height)))))

(defmethod draw ((window window))
  (with-slots (x y) window
    (draw-at window x y)))

(defmethod make-sprite ((window window))
  (with-slots (x y width height texture) window
    (sdl:with-surface (surface (sdl:create-surface width height))
      (with-node-images (tester)
          (fetch-image-node :window :shadow)
        (sdl:blit-surface tester)) ; Dysfunctional blit.
      (setf texture (surface-to-texture surface)))))

;        (draw-at top-01 x (- y (height top-01)))
;        (draw-at corner-right-top (+ x width) (- y (height corner-right-top)))))))