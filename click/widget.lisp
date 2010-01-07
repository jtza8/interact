; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(define-condition invalid-event (error)
  ((event-type :initarg :event-type)))

(defclass widget ()
  ((listeners :initform '()
              :reader listeners)
   (listenable-events :initform '()
                      :initarg :listenable-events)
   (x :initform 0
      :initarg :x)
   (y :initform 0
      :initarg :y)
   (left-margin :initform 0
               :initarg :left-margin)
   (right-margin :initform 0
                :initarg :right-margin)
   (top-margin :initform 0
              :initarg :top-margin)
   (bottom-margin :initform 0
                 :initarg :bottom-margin)
   (width :initform 50
          :initarg :width)
   (height :initform 20
           :initarg :height)
   (texture :initform nil
            :reader texture)))

(defmethod draw-at ((widget widget) x y)
  (with-slots (texture width height left-margin right-margin top-margin
               bottom-margin) widget
    (gl:enable :blend)
    (gl:enable :texture-2d)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:clear :color-buffer-bit)
    (gl:bind-texture :texture-2d texture)
    (let ((total-width (+ width left-margin right-margin))
          (total-height (+ height top-margin bottom-margin)))
      (let ((x (- x left-margin))
            (y (- y top-margin)))
        (gl:with-primitive :quads
          (gl:tex-coord 0 0)
          (gl:vertex x y)
          (gl:tex-coord 1 0)
          (gl:vertex (+ x total-width) y)
          (gl:tex-coord 1 1)
          (gl:vertex (+ x total-width) (+ y total-height))
          (gl:tex-coord 0 1)
          (gl:vertex x (+ y total-height)))))))

(defmethod draw ((widget widget))
  (with-slots (x y) widget
    (draw-at widget x y)))

(defmethod add-listener ((widget widget) listener event)
  (with-slots (listeners listenable-events) widget
    (unless (find event listenable-events)
      (restart-case (error 'invalid-event :event-type event)
        (ignore ()
          :report "Ignore and return from add-listener."
          (return-from add-listener))))
    (if (eq (getf listeners event) nil)
        (progn (push (list listener) listeners)
               (push event listeners))
        (pushnew listener (getf listeners event)))))

(defmethod remove-listener ((widget widget) listener event)
  (with-slots (listeners) widget
    (let ((event-listeners (getf listeners event)))
      (unless event-listeners 
        (restart-case (error 'invalid-event :event-type event)
          (ignore ()
            :report "Ignore and return from remove-listener")))
      (setf (getf listeners event)
            (delete listener (getf listeners event))))))

(defmethod notify-listeners ((widget widget) event &rest args)
  (dolist (listener (getf (slot-value widget 'listeners) event))
    (apply 'event-update `(,listener ,event ,@args))))