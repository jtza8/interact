; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defclass texture-sprite (sprite)
  ((texture :initarg :texture
            :initform (error "No texture name given.")
            :reader texture)))

(define-instance-maker texture-sprite)

(defmethod draw-sprite ((sprite texture-sprite) &key (x 0) (y 0) width height)
  (with-slots (texture (actual-width width) (actual-height height)) sprite
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (when (null width) (setf width actual-width))
    (when (null height) (setf height actual-height))
    (let ((tex-width
           (if (zerop actual-width) 0.0 (float (/ width actual-width))))
          (tex-height
           (if (zerop actual-height) 0.0 (float (/ height actual-height)))))
      (rectangle x y width height
                 :tex-coords
                 (list 0 0 tex-width 0 tex-width tex-height 0 tex-height)))))

(defmethod free ((sprite texture-sprite))
  (with-slots (texture) sprite
    (gl:delete-textures (list texture))))
