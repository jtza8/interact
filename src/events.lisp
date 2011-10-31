; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)
(defparameter *mouse-buttons* #(:left :middle :right))
(defparameter *mouse-pos* '(0 0))
(defparameter *key-state* (make-hash-table))
(defparameter *mouse-button-state* #(nil nil nil))
(declaim (special *main-window* *window-closed*))

;; (defmethod glop:on-close (window)
;;   (setf *window-closed* t)
;;   (send-event *root-container* '(:quit)))

;; (defmethod glop:on-resize (window w h)
;;   (send-event *root-container* `(:window-size :width ,w :height ,h)))

;; (defmethod glop:on-draw (window)
;;   ())

(defun update-event-system (&optional blocking)
  (let ((event (glop:next-event *main-window* :blocking blocking)))
    (typecase event
      (glop:key-event
       (with-accessors ((pressed glop:pressed) (keysym glop:keysym)) event
         (setf (gethash keysym *key-state*) pressed)
         (send-event *root-container*
                     `(,(if pressed :key-down :key-up) :key ,keysym))))
      (glop:button-event
       (with-accessors ((pressed glop:pressed) (button glop:button)) event
         (when (>= 0 button (1- (length *mouse-buttons*)))
           (setf (aref *mouse-button-state* button) pressed))
         (send-event *root-container*
                     `(,(if pressed :mouse-down :mouse-up)
                        :button ,(aref *mouse-buttons* button)))))
      (glop:mouse-motion-event
       (with-accessors ((x glop:x) (y glop:y) (dx glop:dx) (dy glop:dy)) event
         (setf *mouse-pos* (list x y))
         (send-event *root-container*
                     `(:mouse-pos :x ,x :y ,y :dx ,dx :dy ,dy))))
      (glop:close-event
       (setf *window-closed* t)))))

(declaim (inline key-down-p))
(defun key-down-p (&rest keys)
  (loop for key in keys
        when (gethash key *key-state*) return t
        finally (return nil)))

(declaim (inline mouse-down-p))
(defun mouse-down-p (&rest buttons)
  (loop for button in buttons
        when (aref *mouse-button-state*
                   (position button *mouse-buttons*)) return t
        finally (return nil)))

(declaim (inline mouse-pos))
(defun mouse-pos () (values-list *mouse-pos*))

(declaim (inline window-closed-p))
(defun window-closed-p () *window-closed*)

(defun basic-event-handler ()
  (cond ((key-down-p :escape) (setf *quit-state* t))
        ((window-closed-p) (setf *quit-state* t))
        ((key-down-p :f12) (toggle-fullscreen))))

(defmacro do-event-loop (blocking &body body)
  `(loop initially (setf *quit-state* nil)
         until *quit-state*
         do (progn (update-event-system ,blocking) ,@body)))