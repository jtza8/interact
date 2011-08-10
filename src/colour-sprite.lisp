; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defclass colour-sprite (sprite)
  ((colour :initarg :colour
           :initform (error "No colour specified.")
           :reader colour)
   (width :initarg :width
          :initform 0)
   (height :initarg :height
           :initform 0)))

(define-instance-maker colour-sprite)

(defmethod draw-sprite ((sprite colour-sprite) &key (x 0) (y 0) width height)
  (with-slots (colour (actual-width width) (actual-height height)) sprite
    (gl:with-pushed-attrib (:current-bit)
      (when (null width) (setf width actual-width))
      (when (null height) (setf height actual-height))
      (apply #'gl:color colour)
      (gl:bind-texture :texture-2d 0)
      (gl:rect x y (+ x width) (+ y height)))))
