; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(internal define-global-settings *display-settings*)
(define-global-settings (*display-settings*)
  (screen-width 800)
  (screen-height 600)
  (full-screen nil :read)
  (window-title "Lisp")
  (screen-bg-colour '(1 1 1 1)))

(defun prepare-click ()
  (reset *global-stopwatch*)
  (rt:clear-tree *sprite-tree*)
  (set-up-root-container))

(internal update-display-gl)
(defun update-display-gl ()
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 (screen-width) 0 (screen-height) 0 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:viewport 0 0 (screen-width) (screen-height)))

(internal update-display-mode)
(defun update-display-mode ()
  (let ((flags (list sdl:sdl-opengl)))
    (when (full-screen) (push sdl:sdl-fullscreen flags))
    (sdl:window (screen-width) (screen-height)
                :bpp 32
                :flags flags
                :title-caption (window-title)))
  (update-display-gl))

(defun start-display-system ()
  (sdl:init-video)
  (setf cl-opengl-bindings:*gl-get-proc-address*
        #'sdl-cffi::sdl-gl-get-proc-address)
  (update-display-mode)
  (apply #'gl:clear-color (screen-bg-colour))
  (gl:enable :blend)
  (gl:enable :texture-2d)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear :color-buffer-bit)
  (prepare-click))

(defsetf full-screen () (value)
  `(progn 
     (set-full-screen ,value)
     (update-display-mode)))

(defun toggle-fullscreen ()
  (setf (full-screen) (not (full-screen))))

(defun quit-display-system ()
  (delete-all-shaders)
  (delete-all-cameras)
  (sdl:quit-video))

(defun run-display-system ()
  (reset *global-stopwatch* t)
  (reset *frame-stopwatch* t)
  (let ((event (sdl:new-event)) quit)
    (unwind-protect
         (loop 
            until quit
            do (loop
                  until (= 0 (sdl-cffi::sdl-poll-event event))
                  do (progn
                       (when (eq (sdl:get-event-type event) :quit-event)
                         (setf quit t))
                       (send-event *root-container* (parse-sdl-event event))))
            do (progn
                 (send-event *root-container* '(:before-frame))
                 (gl:clear :color-buffer-bit)
                 (draw *root-container*)
                 (gl:flush)
                 (when (> (lap *frame-stopwatch*) 16)
                   (sdl:update-display)
                   (reset *frame-stopwatch* t)
                   (send-event *root-container* '(:display-update)))
                 (send-event *root-container* '(:after-frame))))
      (cffi:foreign-free event))))

(defmacro with-display-system ((&rest args) &body body)
  `(progn 
     ,@(loop for (setting value) on args by #'cddr
             collect `(setf (,(intern (symbol-name setting))) ,value))
     (start-display-system)
     (unwind-protect (progn ,@body
                            (run-display-system))
       (quit-display-system))))

