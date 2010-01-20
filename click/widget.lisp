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
   (x-offset :initform 0
             :initarg :x-offset
             :accessor x-offset)
   (y-offset :initform 0
             :initarg :y-offset
             :accessor y-offset)
   (left-margin :initform 0
               :initarg :left-margin)
   (right-margin :initform 0
                :initarg :right-margin)
   (top-margin :initform 0
              :initarg :top-margin)
   (bottom-margin :initform 0
                 :initarg :bottom-margin)
   (width :initform 50
          :initarg :width
          :reader width)
   (height :initform 20
           :initarg :height
           :reader height)))

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

(defmethod abs-x ((widget widget))
  (with-slots (x x-offset) widget
    (+ x x-offset)))

(defmethod abs-y ((widget widget))
  (with-slots (y y-offset) widget
    (+ y y-offset)))