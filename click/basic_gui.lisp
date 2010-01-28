; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun init-basic-gui (&optional (screen-width 800) (screen-height 600)
                       (full-screen nil))
  (sdl:init-video)
  (let ((flags (list sdl:sdl-opengl)))
    (when full-screen (push sdl:sdl-fullscreen flags))
    (sdl:window screen-width screen-height
                :bpp 32
                :flags flags))
  (setf cl-opengl-bindings:*gl-get-proc-address*
        #'sdl-cffi::sdl-gl-get-proc-address)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 screen-width screen-height 0 0 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:viewport 0 0 screen-width screen-height)
  (gl:clear-color 1 1 1 0.0)
  (gl:clear :color-buffer-bit)
  (init-click))

(defun run-basic-gui ()
  (unwind-protect
       (sdl:with-events (:poll)
         (:mouse-motion-event (:state state :x x :y y :x-rel x-rel :y-rel y-rel)
           (handle-event *window-manager*
                         (list :mouse-move :state state :x x :y y
                               :x-rel x-rel :y-rel y-rel)))
         (:mouse-button-down-event (:button button :state state :x x :y y)  
           (handle-event *window-manager*
                         (list :mouse-down :button button
                               :state state :x x :y y)))
         (:mouse-button-up-event (:button button :state state :x x :y y)  
           (handle-event *window-manager*
                         (list :mouse-up :button button
                               :state state :x x :y y)))
         (:quit-event () t)
         (:idle ()
                (draw *window-manager*)
                (gl:flush)
                (sdl:update-display)))
    (sdl:quit-video)))