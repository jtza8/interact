; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass texture-sprite (sprite)
  ((texture :initarg :texture
            :initform (error "No texture name given.")
            :reader texture)
   (texture-width :initarg :texture-width
                  :initform (error "No texture-width given."))
   (texture-height :initarg :texture-height
                   :initform (error "No texture-height given.")) ))

(defmethod draw-sprite ((sprite texture-sprite) &key (x 0) (y 0))
  (declare (ignore mode width height))
  (with-slots (texture texture-width texture-height width height vbo) sprite
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:matrix-mode :texture)
    (gl:with-pushed-matrix
      (gl:scale (/ 1 texture-width) (/ 1 texture-height) 1)
      (rectangle x y width height
                 :tex-coords (list 0 0 width 0 width height 0 height))))
  (gl:matrix-mode :modelview))

(defmethod free ((sprite texture-sprite))
  (with-slots (texture) sprite
    (gl:delete-textures (list texture))))
