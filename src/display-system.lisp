; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(internal update-display-gl)
(defun update-display-gl ()
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 (width *screen*) (height *screen*) 0 0 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:viewport 0 0 (width *screen*) (height *screen*)))

(internal update-display-mode)
(defun update-display-mode ()
  (let ((flags (list sdl:sdl-opengl)))
    (when (full-screen *screen*) (push sdl:sdl-fullscreen flags))
    (sdl:window (width *screen*) (height *screen*)
                :bpp 32
                :flags flags
                :title-caption (title *screen*)))
  (update-display-gl))

(defun start-display-system ()
  (sdl:init-video)
  (setf cl-opengl-bindings:*gl-get-proc-address*
        #'sdl-cffi::sdl-gl-get-proc-address)
  (update-display-mode)
  (apply #'gl:clear-color (clear-colour *screen*))
  (gl:enable :blend)
  (gl:enable :texture-2d)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear :color-buffer-bit)
  (set-up-root-container)
  (reset *global-watch* t)
  (reset *frame-watch* t)
  (reset *iter-watch* t))

(defun toggle-fullscreen ()
  (setf (full-screen *screen*) (not (full-screen *screen*)))
  (update-display-mode))

(defun quit-display-system ()
  (delete-all-sprites)
  (delete-all-shaders)
  (delete-all-filters)
  (delete-all-cameras)
  (sdl:quit-video)
  (reset *global-watch*))

(defun update-display-system ()
  (when (> (lap *frame-watch*) 16666666) ; 16666666ns => 60fps.
    (send-event *root-container* '(:before-frame))
    (gl:clear :color-buffer-bit)
    (draw *root-container*)
    (gl:flush)
    (sdl:update-display)
    (reset *frame-watch* t)
    (send-event *root-container* '(:after-frame))))

(defmacro with-event-loop ((&optional (event-var (gensym "EVENT-"))
                                      (quit-var (gensym "QUIT-"))
                                      event-code)
                           &body body)
  (let ((event (gensym "SDL-EVENT-")))
   `(let ((,event (sdl:new-event)))
      (unwind-protect
           (loop
              with ,quit-var
              until ,quit-var
              do (loop
                    until (= 0 (sdl-cffi::sdl-poll-event ,event))
                    do (progn
                         (when (eq (sdl:get-event-type ,event) :quit-event)
                           (setf ,quit-var t))
                         (let ((,event-var (parse-sdl-event ,event)))
                           ,event-code
                           (send-event *root-container*
                                       ,event-var))))
              do (progn ,@body))
        (cffi:foreign-free ,event)))))

(defmacro with-display-system ((&key (width 1024)
                                     (height 768)
                                     (full-screen nil)
                                     (clear-colour ''(0.5 0.5 0.5 1.0))
                                     (title "Lisp"))
                               &body body)
  `(progn 
     (setf (width *screen*) ,width
           (height *screen*) ,height
           (full-screen *screen*) ,full-screen
           (clear-colour *screen*) ,clear-colour
           (title *screen*) ,title)
     (start-display-system)
     (unwind-protect (progn ,@body)
       (quit-display-system))))