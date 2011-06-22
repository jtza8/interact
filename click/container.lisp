; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defvar *root-container*)

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

(define-instance-maker container)

(defmethod initialize-instance :after ((container container) &key)
  (forward-standard-events container))


(defmethod tag-igo ((container container) (igo igo) tag)
  (with-slots (tags) container
    (destructuring-bind (fault tag igo)
        (loop for (tag-key tag-value) on tags by #'cddr
              do (cond
                   ((eq tag-key tag)
                    (return `(:duplicate-tag ,tag-key ,tag-value)))
                   ((eq tag-value igo)
                    (return `(:double-tag ,tag-key ,tag-value))))
              finally
                (return '(nil nil nil)))
      (assert (not fault) (tag igo)
              'igo-tag-error :fault fault :tag tag :igo igo))
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
    (assert value (value) 'igo-tag-error :fault :invalid-tag :tag tag)
    value))

(defmethod tag-of ((container container) (igo igo))
  (loop for (key value) on (slot-value container 'tags) by #'cddr
        when (eql value igo) do (return key)
        finally (return nil)))

(defmethod add-igo ((container container) (igo igo) &optional tag)
  (with-slots (igos) container
    (loop for existing across igos
          when (eq existing igo) do (return-from add-igo))
    (setf (parent igo) container)
    (vector-push-extend igo igos))
  (subscribe container igo)
  (when (keywordp tag)
    (tag-igo container igo tag)))

(defmethod remove-igo ((container container) igo &key (unsubscribes t))
  (with-slots (igos tags) container
    (setf igos (loop with new-vector = (make-array (length igos) 
                                                   :fill-pointer 0
                                                   :adjustable t)
                     for existing across igos
                     unless (eq existing igo) 
                       do (vector-push-extend existing new-vector)
                     finally (return new-vector)))
    (remove-tag container igo)
    (when unsubscribes
      (unsubscribe container igo))))

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
                 remove-root-listener add-to-root
                 remove-from-root igo-of-root
                 root-tag-of root-tag-igo
                 root-remove-tag))
(internal set-up-root-container)
(defun set-up-root-container ()
  (setf *root-container* (make-instance 'container :clipping nil)))
(defun add-root-listener (listener &optional event-type)
  (subscribe *root-container* listener event-type))
(defun remove-root-listener (listener &optional event-type)
  (unsubscribe *root-container* listener event-type))
(defun add-to-root (igo &optional tag)
  (add-igo *root-container* igo tag))
(defun remove-from-root (igo &key (unsubscribes t))
  (remove-igo *root-container* igo :unsubscribes unsubscribes))
(defun igo-of-root (tag)
  (igo-of *root-container* tag))
(defun root-tag-of (igo)
  (tag-of *root-container* igo))
(defun root-tag-igo (igo tag)
  (tag-igo *root-container* igo tag))
(defun root-remove-tag (identifier)
  (remove-tag *root-container* identifier))
