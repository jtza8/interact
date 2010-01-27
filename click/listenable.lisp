; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(define-condition invalid-event (error)
  ((reason :initarg :reason
           :initform "Invalid event"
           :reader reason)
   (event :initarg :event
          :reader event))
  (:report (lambda (condition stream)
             (format stream "~A: ~S" (reason condition) (event condition)))))

(define-condition invalid-event-type (invalid-event)
  ((reason :initform "Invalid event type")))

(defclass listenable ()
  ((listeners :initform '()
              :reader listeners)
   (listenable-events :initform '()
                      :initarg :listenable-events
                      :reader listenable-events)))

(defmacro event-type (event) `(car ,event))
(defmacro event-data (event) `(cdr ,event))

(defmethod add-listener ((listenable listenable) listener event-type)
  (with-slots (listeners listenable-events) listenable
    (assert (find event-type listenable-events) (event-type)
            'invalid-event-type
            :reason "Unlistenable event type"
            :event event-type)
    (unless (select-handler listener event-type)
      (warn "~S requested to listen to event ~S but doesn't provide a handler"
            listener event-type))
    (if (eq (getf listeners event-type) nil)
        (progn (push (list listener) listeners)
               (push event-type listeners))
        (pushnew listener (getf listeners event-type)))))

(defmethod remove-listener ((listenable listenable) listener event-type)
  (with-slots (listeners) listenable
    (let ((event-listeners (getf listeners event-type)))
      (assert event-listeners (event-type)
              'invalid-event-type
              :reason "Nonexistent event type"
              :event event-type)
      (setf (getf listeners event-type)
            (delete listener (getf listeners event-type))))))

(defmethod handle-event ((listenable listenable) event)
  (with-slots (listenable-events listeners) listenable
    (assert (find (event-type event) listenable-events) ()
            'invalid-event
            :reason "Unsupported event"
            :event event)
    (dolist (listener (getf listeners (event-type event)))
      (let ((handler (select-handler listener (event-type event))))
        (unless (null handler)
          (funcall handler listener event))))))