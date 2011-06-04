; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass colour-sprite (sprite)
  ((colour :initarg :colour
           :initform (error "No colour specified.")
           :reader colour)
   (width :initarg :width
          :initform 0)
   (height :initarg :height
           :initform 0)))

(define-instance-maker colour-sprite)

(defmethod draw-sprite ((sprite colour-sprite)
                        &key (x 0) (y 0))
  (with-slots (colour width height) sprite
    (gl:push-attrib :current-bit)
    (apply #'gl:color colour)
    (gl:rect x y (+ x width) (+ y height))
    (gl:pop-attrib)))
