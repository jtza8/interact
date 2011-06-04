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
   (provided-events :initform '()
                    :initarg :provided-events
                    :reader provided-events)))

(defmethod provide-events ((listenable listenable) &rest events)
  (with-slots (provided-events) listenable
    (if (null provided-events)
        (setf provided-events (delete-duplicates events))
        (setf provided-events 
              (delete-duplicates (nconc provided-events events))))))

(declaim (inline event-type event-data))
(defun event-type (event) (car event))
(defun event-data (event) (cdr event))

(defmacro with-event-keys (keys event &body body)
  (let ((data (gensym "event")))
    `(let* ((,data (cdr ,event))
            ,@(loop for key in keys collect
                   `(,key (getf ,data ,(intern (symbol-name key) "KEYWORD")))))
       ,@body)))

(defmethod add-listener ((listenable listenable) listener &optional event-type)
  (with-slots (listeners provided-events) listenable
    (when (null event-type)
      (loop for (event) on (desired-events listener) by #'cddr
            do (when (find event provided-events)
                 (add-listener listenable listener event)))
      (return-from add-listener))
    (assert (find event-type provided-events) (event-type)
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

(defmethod remove-listener ((listenable listenable) listener 
                            &optional event-type)
  (when (null event-type)
    (loop for (event nil) on (desired-events listener) by #'cddr
          do (remove-listener listenable listener event))
    (return-from remove-listener))
  (with-slots (listeners) listenable
    (let ((event-listeners (getf listeners event-type)))
      (assert event-listeners (event-type)
              'invalid-event-type
              :reason "Nonexistent event type"
              :event event-type)
      (setf (getf listeners event-type)
            (delete listener (getf listeners event-type))))))

(defmethod send-event ((listenable listenable) event)
  (with-slots (provided-events listeners) listenable
    (assert (find (event-type event) provided-events) ()
            'invalid-event
            :reason "Unsupported event"
            :event event)
    (dolist (listener (getf listeners (event-type event)))
      (let ((handler (select-handler listener (event-type event))))
        (if (not (null handler))
            (funcall handler listener event)
            (warn "~S listens for event ~S but doesn't specify a handler"
                  listener (event-type event)))))))
