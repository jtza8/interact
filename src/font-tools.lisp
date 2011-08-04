; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defun render-text (text &key (type-face sdl:*default-font*)
                    (justify "left") (colour sdl:*black*))
  (let ((width (sdl:get-font-size text :size :w :font type-face))
        (height (sdl:get-font-size text :size :h :font type-face)))
    (sdl:with-surface (surface
                       (sdl:create-surface width height :bpp 32 :pixel-alpha t))
      (sdl:draw-string-blended-* text 0 0
                                 :justify justify :surface surface
                                 :font type-face :colour colour)
      (sdl-base::with-pixel (pix (sdl:fp surface))
        (let ((texture (car (gl:gen-textures 1))))
          (gl:bind-texture :texture-2d texture)
          (gl:tex-parameter :texture-2d :texture-min-filter :linear)
          (gl:tex-image-2d :texture-2d 0 :rgba width height 0 :rgba
                           :unsigned-byte (sdl-base::pixel-data pix))
          (make-instance 'texture-sprite :texture texture
                         :width width :height height))))))
