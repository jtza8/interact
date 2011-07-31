; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defparameter *global-watch* (make-instance 'watch))
(defparameter *frame-watch* (make-instance 'watch :parent *global-watch*))
(defparameter *iter-watch* (make-instance 'watch :parent *global-watch*))

(internal define-global-settings *display-settings*)
(define-global-settings (*display-settings*)
  (screen-width 800)
  (screen-height 600)
  (full-screen nil :read)
  (window-title "Lisp")
  (screen-colour '(1 1 1 1)))

(defun prepare-click ()
  (set-up-root-container))

(internal update-display-gl)
(defun update-display-gl ()
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 (screen-width) (screen-height) 0 0 1)
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
  (apply #'gl:clear-color (screen-colour))
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
  (delete-all-sprites)
  (delete-all-shaders)
  (delete-all-filters)
  (delete-all-cameras)
  (sdl:quit-video)
  (reset *global-watch*))

(defun run-display-system ()
  (reset *global-watch* t)
  (reset *frame-watch* t)
  (reset *iter-watch* t)
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
                 (when (> (lap *frame-watch*) 16666666) ; 16666666ns => 60fps.
                   (send-event *root-container* '(:before-frame))
                   (gl:clear :color-buffer-bit)
                   (draw *root-container*)
                   (gl:flush)
                   (sdl:update-display)
                   (reset *frame-watch* t)
                   (send-event *root-container* '(:display-update))
                   (send-event *root-container* '(:after-frame)))
                 (send-event *root-container* `(:loop-iteration
                                                :time ,(lap *iter-watch* :sec)))
                 (reset *iter-watch* t)))
      (cffi:foreign-free event))))

(defmacro with-display-system ((&rest args) &body body)
  `(progn 
     ,@(loop for (setting value) on args by #'cddr
             collect `(setf (,(intern (symbol-name setting) :click)) ,value))
     (start-display-system)
     (unwind-protect (progn ,@body
                            (run-display-system))
       (quit-display-system))))

