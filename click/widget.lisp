; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(define-condition invalid-event (error)
  ((event :initarg :event
          :reader event))
  (:report (lambda (condition stream)
             (format stream "Invalid event: ~S" (event condition)))))

(defclass widget ()
  ((listeners :initform '()
              :reader listeners)
   (listenable-events :initform '()
                      :initarg :listenable-events
                      :reader listenable-events)
   (subscribe-events :initform '()
                     :reader subscribe-events)
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

(defmethod abs-x ((widget widget))
  (with-slots (x x-offset) widget
    (+ x x-offset)))

(defmethod abs-y ((widget widget))
  (with-slots (y y-offset) widget
    (+ y y-offset)))

(defmacro event-type (event) `(car ,event))
(defmacro event-data (event) `(cdr ,event))

(defmethod add-listener ((widget widget) listener event-type)
  (with-slots (listeners listenable-events) widget
    (assert (find event-type listenable-events) (event-type)
            'invalid-event :event (list event-type))
    (if (eq (getf listeners event-type) nil)
        (progn (push (list listener) listeners)
               (push event-type listeners))
        (pushnew listener (getf listeners event-type)))))

(defmethod remove-listener ((widget widget) listener event-type)
  (with-slots (listeners) widget
    (let ((event-listeners (getf listeners event-type)))
      (assert event-listeners (event-type)
              'invalid-event :event (list event-type))
      (setf (getf listeners event-type)
            (delete listener (getf listeners event-type))))))

(defmethod get-event-handler ((widget widget) event) nil)

(defmethod handle-event ((widget widget) event)
  (with-slots (listenable-events) widget
    (assert (find (event-type event) listenable-events) ()
            'invalid-event :event event))  
  (dolist (listener (getf (slot-value widget 'listeners) (car event)))
    (funcall (get-event-handler widget event) listener event)))
