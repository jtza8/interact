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
   (igos :initform (make-array 5 :fill-pointer 0 :adjustable t)
         :reader igos)
   (clipping :initarg :clipping
             :initform t
             :accessor clipping)
   (background :initform nil
               :initarg :background
               :reader background)
   (tags :initform '())))

(defmethod initialize-instance :after ((container container) &key)
  (provide-events container :display-state :key-down :key-up :mouse-motion
                  :mouse-button-down :mouse-button-up :parent-move 
                  :joy-axis-motion :joy-hat-motion :joy-ball-motion
                  :joy-button-down :joy-button-up :quit :sys-wm-event
                  :display-resize :display-exposure :user-event
                  :before-frame :after-frame)
  (desire-events container :display-state #'send-event :key-down #'send-event 
                 :key-up #'send-event :mouse-motion #'send-event
                 :mouse-button-down #'send-event :mouse-button-up #'send-event 
                 :parent-move #'send-event :joy-axis-motion #'send-event
                 :joy-hat-motion #'send-event :joy-ball-motion #'send-event
                 :joy-button-down #'send-event :joy-button-up #'send-event
                 :quit #'send-event :sys-wm-event #'send-event 
                 :display-resize #'send-event :display-exposure #'send-event
                 :user-event #'send-event :before-frame #'send-event
                 :after-frame #'send-event))

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
                  unless (eq (if (typep identifier 'igo)
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
  (with-slots (igos) container
    (loop for existing across igos
          when (eq existing igo) do (return-from add-igo))
    (setf (parent igo) container)
    (vector-push-extend igo igos))
  (add-listener container igo)
  (when (keywordp tag)
    (tag-igo container igo tag)))

(defmethod remove-igo ((container container) igo &key (remove-listeners t))
  (with-slots (igos tags) container
    (setf igos (loop with new-vector = (make-array (length igos) 
                                                   :fill-pointer 0
                                                   :adjustable t)
                     for existing across igos
                     unless (eq existing igo) 
                       do (vector-push-extend existing new-vector)
                     finally (return new-vector)))
    (remove-tag container igo)
    (when remove-listeners
      (remove-listener container igo))))

(defmethod draw ((container container))
  (with-slots (visible) container
    (unless visible
      (return-from draw))
    (draw-background container)
    (loop for igo across (slot-value container 'igos)
       do (draw igo))))

(defmethod draw-background ((container container))
  (with-slots (width height background) container
    (when (null background)
      (return-from draw-background))
    (draw-sprite background :width width :height height :mode :tile)))


(declaim (inline set-up-root-container add-root-listener 
                 remove-root-listener add-root-igo
                 remove-root-igo igo-of-root
                 root-tag-of root-tag-igo
                 root-remove-tag))
(internal set-up-root-container)
(defun set-up-root-container ()
  (setf *root-container* (make-instance 'container :clipping nil)))
(defun add-root-listener (listener &optional event-type)
  (add-listener *root-container* listener event-type))
(defun remove-root-listener (listener &optional event-type)
  (remove-listener *root-container* listener event-type))
(defun add-root-igo (igo &optional tag)
  (add-igo *root-container* igo tag))
(defun remove-root-igo (igo &key (remove-listeners t))
  (remove-igo *root-container* igo :remove-listeners remove-listeners))
(defun igo-of-root (tag)
  (igo-of *root-container* tag))
(defun root-tag-of (igo)
  (tag-of *root-container* igo))
(defun root-tag-igo (igo tag)
  (tag-igo *root-container* igo tag))
(defun root-remove-tag (identifier)
  (remove-tag *root-container* identifier))