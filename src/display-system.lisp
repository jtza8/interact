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
  (glfw:close-window)
  (glfw:open-window (width *screen*) (height *screen*) 0 0 0 0 0 0
                    (if (full-screen *screen*) :fullscreen :window))
  (glfw:set-window-title (title *screen*))
  (update-display-gl))

(defun start-display-system ()
  (update-display-mode)
  (apply #'gl:clear-color (clear-colour *screen*))
  (gl:enable :blend)
  (gl:enable :texture-2d)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear :color-buffer-bit)
  (set-up-root-container)
  (set-event-callbacks)
  (reset *global-watch* t)
  (reset *frame-watch* t)
  (reset *iter-watch* t))

(defun toggle-fullscreen ()
  (setf (full-screen *screen*) (not (full-screen *screen*)))
  (update-display-mode))

(defun quit-display-system ()
  (delete-all-sprites)
  ;; (delete-all-shaders)
  ;; (delete-all-filters)
  ;; (delete-all-cameras)
  (reset *global-watch*))

(defun update-display-system ()
  (when (> (lap *frame-watch*) 16666666) ; 16666666ns => 60fps.
    (send-event *root-container* '(:before-frame))
    (gl:clear :color-buffer-bit)
    (draw *root-container*)
    (gl:flush)
    (glfw:swap-buffers)
    (reset *frame-watch* t)
    (send-event *root-container* '(:after-frame))))

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
     (glfw:with-init ()
       (start-display-system)
       (unwind-protect (progn ,@body)
         (quit-display-system)))))