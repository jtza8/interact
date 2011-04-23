; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun prepare-click ()
  (reset *global-stopwatch*)
  (rt:clear-tree *sprite-tree*)
  (set-up-root-container))

(defun start-display-system (&key (width 800) (height 600) (full-screen nil)
                             (bg-color '(1 1 1 0)) (title "Lisp"))
  (sdl:init-video)
  (let ((flags (list sdl:sdl-opengl)))
    (when full-screen (push sdl:sdl-fullscreen flags))
    (sdl:window width height
                :bpp 32
                :flags flags
                :title-caption title))
  (setf (sdl:frame-rate) 60
        cl-opengl-bindings:*gl-get-proc-address*
        #'sdl-cffi::sdl-gl-get-proc-address)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 width height 0 0 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:viewport 0 0 width height)
  (apply #'gl:clear-color bg-color)
  (gl:enable :blend)
  (gl:enable :texture-2d)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear :color-buffer-bit)
  (prepare-click))

(defun quit-display-system ()
  (sdl:quit-video))

(defun run-display-system ()
  (start *global-stopwatch*)
  (let ((event (sdl:new-event))
        (frame-watch (make-instance 'stopwatch)) quit)
    (start frame-watch)
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
                 (gl:clear :color-buffer-bit)
                 (send-event *root-container* '(:update-frame))
                 (draw *root-container*)
                 (gl:flush)
                 (sdl:update-display)
                 (sleep (max (- 1/60 (/ (lap frame-watch) 1000)) 0))
                 (reset frame-watch :auto-start t)))
      (cffi:foreign-free event))))

(defmacro with-display-system ((&rest args) &body body)
  `(progn (start-display-system ,@args)
          (unwind-protect (progn ,@body 
                                 (run-display-system))
            (quit-display-system))))

