; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(declaim (inline key-down-p))
(defun key-down-p (&rest keys)
  (loop for key in keys
        when (eq (glfw:get-key key) :press) return t
        finally (return nil)))

(declaim (inline mouse-down-p))
(defun mouse-down-p (&rest buttons)
  (loop for button in buttons
        when (eq (glfw:get-mouse-button button) :press) return t
        finally (return nil)))

(declaim (inline mouse-wheel-pos mouse-pos))
(defun mouse-wheel-pos () (glfw:get-mouse-wheel))
(defun mouse-pos () (glfw:get-mouse-pos))

(defun set-event-callbacks ()
  (flet ((key-callback (key state)
           (send-event *root-container*
                       `(,(if (eq state :press) :key-down :key-up) :key ,key)))
         (char-callback (char state)
           (send-event *root-container*
                       `(,(if (eq state :press) :char-down :char-up) 
                          :key ,char)))
         (mouse-button-callback (button state)
           (send-event *root-container*
                       `(,(if (eq state :press) :mouse-down :mouse-up)
                          :button ,button)))
         (mouse-pos-callback (x y)
           (send-event *root-container* `(:mouse-pos :x ,x :y ,y)))
         (mouse-wheel-callback (pos)
           (send-event *root-container* `(:mouse-wheel :pos ,pos)))
         ;; (window-close-callback ()
         ;;   (send-event *root-container* '(:window-close)))
         (window-size-callback (width height)
           (send-event *root-container* `(:window-size 
                                          :width ,width
                                          :height ,height)))
         (window-refresh-callback ()
           (send-event *root-container* `(:window-refresh))))
    (glfw:set-key-callback #'key-callback)
    (glfw:set-char-callback #'char-callback)
    (glfw:set-mouse-button-callback #'mouse-button-callback)
    (glfw:set-mouse-pos-callback #'mouse-pos-callback)
    (glfw:set-mouse-wheel-callback #'mouse-wheel-callback)
    ;; (glfw:set-window-close-callback #'window-close-callback)
    (glfw:set-window-size-callback #'window-size-callback)
    (glfw:set-window-refresh-callback #'window-refresh-callback)))

(defun basic-event-handler ()
  (cond ((key-down-p :esc) (setf *quit-state* t))
        ((not (glfw:get-window-param :opened)) (setf *quit-state* t))
        ((key-down-p :f12) (toggle-fullscreen))))

(defmacro with-event-loop (&body body)
  `(loop initially (setf *quit-state* nil)
         until *quit-state*
         do (progn ,@body)))