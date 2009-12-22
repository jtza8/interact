; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(define-condition unlistenable-event (error)
  ((event-type :initvar :event-type)))

(defclass widget ()
  (listeners :initform '())
  (listenable-events :initform '()
                     :initvar :listenable-events))

(defmethod add-listener ((widget widget) listener event)
  (with-slots (listeners) widget
    (unless (find event listenable-events)
      (restart-case (error 'unlistenable-event :event-type event)
        (ignore ()
          :report "Ignore and return from add-listener."
          (return-from add-listener))))
    (if (eq (getf listeners event) nil)
        (progn (push (list listener) listeners)
               (push event listeners))
        (pushnew listener (getf listeners event)))))

(defmethod remove-listener ((widget widget) listener)
  (with-slots (listeners) widget
    (setf listeners (delete listener listeners))))

(defmethod notify-listeners ((widget widget) event &rest args)
  (dolist (listener (getf (slot-value widget 'listeners) event))
    (apply 'event-update (cons event args))))