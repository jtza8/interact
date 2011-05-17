; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass texture-sprite (sprite)
  ((texture :initarg :texture
            :initform (error "No texture name given.")
            :reader texture)
   (texture-width :initarg :texture-width)
   (texture-height :initarg :texture-height)))

(defmethod initialize-instance :after ((sprite texture-sprite) &key)
  (with-slots (width texture-width height texture-height) sprite
    (setf texture-width (if (null texture-width) 
                            (power-size width 2)
                            texture-width)
          texture-height (if (null texture-height)
                             (power-size height 2)
                             texture-height))))

(defmethod draw-sprite ((sprite texture-sprite) &key (x 0) (y 0))
  (let ((matrix-mode (gl:get-integer :matrix-mode)))
    (with-slots (texture texture-width texture-height width height) sprite
      (gl:bind-texture :texture-2d texture)
      (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
      (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
      (gl:matrix-mode :texture)
      (gl:with-pushed-matrix
        (gl:scale (/ 1 texture-width) (/ 1 texture-height) 1)
        (rectangle x y width height
                   :tex-coords (list 0 0 width 0 width height 0 height))))
    (gl:matrix-mode matrix-mode)))

(defmethod free ((sprite texture-sprite))
  (with-slots (texture) sprite
    (gl:delete-textures (list texture))))
