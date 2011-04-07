; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defvar *root-container*)

(define-condition tag-error (error)
  ((fault :initarg :fault
          :initform :fault)
   (tag :initarg :tag
        :initform nil)
   (igo :initarg :igo
        :initform nil))
  (:report (lambda (condition stream)
             (with-slots (fault tag igo) condition
               (case fault
                 (:tag (format stream "Tag ~s for igo ~s must be unique"
                               tag igo))
                 (:igo (format stream "Widget ~s already has tag ~s"
                                  igo tag))
                 (:invalid-tag (format stream "Couldn't find tag ~s" tag))
                 (otherwise (format stream "Unknown fault: ~s" fault)))))))

(defclass container (igo)
  ((visible :initarg :visible
            :initform t)
   (igos :initform '()
         :reader igos)
   (height :initform -1)
   (width :initform -1)
   (background :initform nil
               :reader background)
   (tags :initform '())))

(defmethod initialize-instance :after ((container container) &key)
  (provide-events container :display-state :key-down :key-up :mouse-motion
                  :mouse-button-down :mouse-button-up :parent-move 
                  :joy-axis-motion :joy-hat-motion :joy-ball-motion
                  :joy-button-down :joy-button-up :quit :sys-wm-event
                  :display-resize :display-exposure :user-event)
  (desire-events container :display-state #'send-event :key-down #'send-event 
                 :key-up #'send-event :mouse-motion #'send-event
                 :mouse-button-down #'send-event :mouse-button-up #'send-event 
                 :parent-move #'send-event :joy-axis-motion #'send-event
                 :joy-hat-motion #'send-event :joy-ball-motion #'send-event
                 :joy-button-down #'send-event :joy-button-up #'send-event
                 :quit #'send-event :sys-wm-event #'send-event 
                 :display-resize #'send-event :display-exposure #'send-event
                 :user-event #'send-event))

(defmethod tag-igo ((container container) (igo igo) tag)
  (with-slots (tags) container
    (multiple-value-bind (fault tag igo)
        (loop
           for (tag-key tag-value) on tags by #'cddr do
             (cond
               ((eq tag-key tag)
                (return (values :tag tag-key tag-value)))
               ((eq tag-value igo)
                (return (values :igo tag-key tag-value))))
           finally
             (return (values nil nil nil)))
      (assert (not fault) (tag igo)
              'tag-error :fault fault :tag tag :igo igo))
    (setf (getf tags tag) igo)))

(defmethod remove-tag ((container container) identifier)
  (with-slots (tags) container
    (when (null tags) (return-from remove-tag))
    (setf tags (loop for (key value) on tags by #'cddr
                  unless (eq (if (subtypep (type-of identifier) 'igo) 
                                 value key)
                             identifier)
                  collect key and collect value))))

(defmethod igo-of ((container container) tag)
  (let ((value (getf (slot-value container 'tags) tag)))
    (assert value (value) 'tag-error :fault :invalid-tag :tag tag)
    value))

(defmethod tag-of ((container container) (igo igo))
  (loop for (key value) on (slot-value container 'tags) by #'cddr
        when (eql value igo) do (return key)
        finally (return nil)))

(defmethod add-igo ((container container) igo &optional tag)
  (check-type igo igo)
  (setf (parent igo) container)
  (with-slots (igos) container
    (pushnew igo igos))
  (add-listener container igo)
  (when (keywordp tag)
    (tag-igo container igo tag)))

(defmethod remove-igo ((container container) igo &key (remove-listeners t))
  (with-slots (igos tags) container
    (setf igos (delete-if (lambda (other-igo) (eq igo other-igo))
                          igos))
    (remove-tag container igo)
    (when remove-listeners
      (dolist (event-type (desired-events igo))
        (remove-listener container igo event-type)))))

(defmethod draw ((container container))
  (with-slots (visible x y width height) container
    (unless visible
      (return-from draw))
    (with-translate (x y)
      (with-clipping (x y width height)
        (print (gl:get-integer :scissor-box))
        (draw-background container)
        (dolist (igo (slot-value container 'igos))
          (draw igo))))))

(defmethod draw-background ((container container))
  (with-slots (x y width height background) container
    (when (null background)
      (return-from draw-background))
    (draw-at background x y :width width :height height :mode :tile)))

(defun set-up-root-container ()
  (setf *root-container* (make-instance 'container)))
(set-up-root-container)