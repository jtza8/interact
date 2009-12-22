; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass click-wm ()
  ((screen-width :initform 800
                 :initarg :screen-width
                 :reader screen-width)
   (screen-height :initform 600
                  :initarg :screen-height
                  :reader screen-height)))

(defmethod initialize-instance :after ((click-wm click-wm) &key)
  (with-slots (screen-width screen-height) click-wm
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 screen-width  screen-height 0 0 1)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:viewport 0 0 screen-width screen-height)))

(defun start-click-wm (&optional (screen-width 800) (screen-height 600)
                       (full-screen nil))
  (assert (eq sdl:*default-display* nil))
  (sdl:with-init (sdl:sdl-init-video)
    (let ((flags (list sdl:sdl-opengl)))
      (when full-screen (push sdl:sdl-fullscreen flags))
      (sdl:window screen-width screen-height
                  :bpp 32
                  :flags flags))
    (setf cl-opengl-bindings:*gl-get-proc-address*
          #'sdl-cffi::sdl-gl-get-proc-address)
    (gl:clear-color 0.0 0.0 0.0 0.0)
    (let ((click-wm (make-instance 'click-wm
                                   :screen-width screen-width
                                   :screen-height screen-height)))
      (sdl:with-events ()
        (:quit-event () t)
        (:idle ()
               ())))))