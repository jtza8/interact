; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click-examples)

(defparameter *width* 640)
(defparameter *height* 480)

(defun prepare-window (width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((ratio (coerce (/ height width) 'double-float)))
    (gl:ortho -1.0d0 1.0d0 (- ratio) ratio -1.0d0 1.0d0))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:viewport 0 0 width height))

(defun draw-square ()
  (gl:load-identity)
  (gl:clear :color-buffer-bit)
  (gl:with-primitive :polygon
    (gl:color 1.0 0.0 0.0)
    (gl:vertex -0.5 -0.5)
    (gl:color 0.0 1.0 0.0)
    (gl:vertex -0.5 0.5)
    (gl:color 0.0 1.0 1.0)
    (gl:vertex 0.5 0.5)
    (gl:color 0.0 0.0 1.0)
    (gl:vertex 0.5 -0.5))
  (gl:flush)
  (sdl:update-display))

(defun hello-world ()
  (sdl:with-init (sdl:sdl-init-video)
    (sdl:window *width* *height*
                :bpp 32
                :flags '(sdl:sdl-opengl sdl:sdl-resizable))
    (setf cl-opengl-bindings:*gl-get-proc-address*
          #'sdl-cffi::sdl-gl-get-proc-address)
    (gl:clear-color 0.0 0.0 0.0 0.0)
    (prepare-window *width* *height*)
    (sdl:with-events ()
      (:quit-event () t)
      (:video-resize-event (:w width :h height)
                           (prepare-window width height))
      (:idle ()
             (draw-square)))))