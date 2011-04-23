; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass texture-sprite (sprite)
  ((texture :initarg :texture
            :initform (error "No texture name given.")
            :reader texture)))

(defmethod draw-sprite ((sprite texture-sprite) x y &key width height (mode :tile))
  (declare (ignore mode))
  (with-slots ((normal-width width) (normal-height height) texture) sprite
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (when (null width) (setf width normal-width))
    (when (null height) (setf height normal-height))
    (rectangle x y width height
               :tex-coords (list 0 0
                                 (/ width normal-width) 0
                                 (/ width normal-width) (/ height normal-height)
                                 0 (/ height normal-height)))))
