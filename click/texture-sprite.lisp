; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass texture-sprite (sprite)
  ((texture :initarg :texture
            :initform (error "No texture name given.")
            :reader texture)))

(define-instance-maker texture-sprite)

(defmethod draw-sprite ((sprite texture-sprite) &key (x 0) (y 0))
  (with-slots (texture width height) sprite
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (rectangle x y width height
               :tex-coords (list 0 0 1 0 1 1 0 1))))

(defmethod free ((sprite texture-sprite))
  (with-slots (texture) sprite
    (gl:delete-textures (list texture))))
