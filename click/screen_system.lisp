; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun init-screen-system (&key (width 800) (height 600) (full-screen nil)
                           (bg-colour '(1 1 1 0)) (window-title "Lisp")
                           sprite-path)
  (cond ((null sprite-path) (error "No sprite path specified."))
        ((not (cl-fad:directory-exists-p sprite-path))
         (error "Sprite path invalid.")))
  (setf *sprite-path* sprite-path)
  (sdl:init-video)
  (let ((flags (list sdl:sdl-opengl)))
    (when full-screen (push sdl:sdl-fullscreen flags))
    (sdl:window width height
                :bpp 32
                :flags flags
                :title-caption window-title))
  (setf cl-opengl-bindings:*gl-get-proc-address*
        #'sdl-cffi::sdl-gl-get-proc-address)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 width height 0 0 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:viewport 0 0 width height)
  (apply #'gl:clear-color bg-colour)
  (gl:enable :blend)
  (gl:enable :texture-2d)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear :color-buffer-bit)
  (init-click))

(defun run-screen-system ()
  (unwind-protect
       (sdl:with-events (:poll)
         (:mouse-motion-event (:state state :x x :y y :x-rel x-rel :y-rel y-rel)
           (send-event *screen-manager*
                       (list :mouse-move :state state :x x :y y
                             :x-rel x-rel :y-rel y-rel)))
         (:mouse-button-down-event (:button button :state state :x x :y y)  
           (send-event *screen-manager*
                       (list :mouse-down :button button
                             :state state :x x :y y)))
         (:mouse-button-up-event (:button button :state state :x x :y y)  
           (send-event *screen-manager*
                       (list :mouse-up :button button
                             :state state :x x :y y)))
         (:key-down-event (:key key)
           (cond ((sdl:key= key :sdl-key-escape) (sdl:push-quit-event))
                 ((sdl:key= key :sdl-key-F12)
                  (sdl:resize-window (sdl:width sdl:*default-display*)
                                     (sdl:height sdl:*default-display*)
                                     :fullscreen (not (sdl:fullscreen-p)))
                  (gl:clear :color-buffer-bit))))
         (:quit-event () t)
         (:idle ()
                (gl:clear :color-buffer-bit)
                (draw *screen-manager*)
                (gl:flush)
                (sdl:update-display)))
    (sdl:quit-video)))

