; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)
(defparameter *main-window* nil)
(defparameter *window-closed* nil)

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
  (when *main-window*
    (glop:close-window *main-window*))
  (setf *main-window*
        (glop:create-window (title *screen*) (width *screen*) (height *screen*)
                            :fullscreen (full-screen *screen*)
                            :red-size 8
                            :green-size 8
                            :blue-size 8
                            :alpha-size 8
                            :depth-size 0))
  (update-display-gl)
  (glop:show-window *main-window*))

(defun start-display-system ()
  (setf *window-closed* nil)
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
  ;; (delete-all-shaders)
  ;; (delete-all-filters)
  ;; (delete-all-cameras)
  (setf *window-closed* nil
        *mouse-pos* '(0 0)
        *key-state* (make-hash-table)
        *mouse-button-state* #(nil nil nil))
  (glop:close-window *main-window*))

(defun update-display-system ()
  (when (> (lap *frame-watch*) 16666666) ; 16666666ns => 60fps.
    (send-event *root-container* '(:before-frame))
    (gl:clear :color-buffer-bit)
    (draw *root-container*)
    (gl:flush)
    (assert *main-window* ()
            "Can't call update-display-system without a display to update.")
    (glop:swap-buffers *main-window*)
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
     (start-display-system)
     (unwind-protect (progn ,@body)
       (quit-display-system))))