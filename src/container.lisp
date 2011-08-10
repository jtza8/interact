; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defvar *root-container*)

(defclass container (widget)
  ((visible :initarg :visible
            :initform t)
   (widgets :initform (make-array 5 :fill-pointer 0 :adjustable t)
            :reader widgets)
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

(defmethod tag-widget ((container container) (widget widget) tag)
  (with-slots (tags) container
    (destructuring-bind (fault tag widget)
        (loop for (tag-key tag-value) on tags by #'cddr
              do (cond
                   ((eq tag-key tag)
                    (return `(:duplicate-tag ,tag-key ,tag-value)))
                   ((eq tag-value widget)
                    (return `(:double-tag ,tag-key ,tag-value))))
              finally
                (return '(nil nil nil)))
      (assert (not fault) (tag widget)
              'widget-tag-error :fault fault :tag tag :widget widget))
    (setf (getf tags tag) widget)))

(defmethod remove-tag ((container container) identifier)
  (with-slots (tags) container
    (when (null tags) (return-from remove-tag))
    (setf tags (loop for (key value) on tags by #'cddr
                  unless (eq (if (typep identifier 'widget)
                                 value key)
                             identifier)
                  collect key and collect value))))

(defmethod widget-of ((container container) tag)
  (let ((value (getf (slot-value container 'tags) tag)))
    (assert value (value) 'widget-tag-error :fault :invalid-tag :tag tag)
    value))

(defmethod tag-of ((container container) (widget widget))
  (loop for (key value) on (slot-value container 'tags) by #'cddr
        when (eql value widget) do (return key)
        finally (return nil)))

(defmethod add-widget ((container container) (widget widget) &optional tag)
  (with-slots (widgets) container
    (loop for existing across widgets
          when (eq existing widget) do (return-from add-widget))
    (setf (parent widget) container)
    (vector-push-extend widget widgets))
  (subscribe container widget)
  (when (keywordp tag)
    (tag-widget container widget tag)))

(defmethod remove-widget ((container container) widget &key (unsubscribes t))
  (with-slots (widgets tags) container
    (setf widgets
          (loop with new-vector = (make-array (length widgets) 
                                              :fill-pointer 0
                                              :adjustable t)
                for existing across widgets
                unless (eq existing widget) 
                do (vector-push-extend existing new-vector)
                finally (return new-vector)))
    (remove-tag container widget)
    (when unsubscribes
      (unsubscribe container widget))))

(internal define-order-method)
(defmacro define-order-method (name (&optional (i 'i) (widgets 'widgets))
                               i-test &body body)
  `(defmethod ,name ((container container) (target-widget widget))
     (with-slots ((,widgets widgets)) container
       (let ((,i (position target-widget widgets :test #'eq)))
         (when (and (not (null ,i)) ,i-test)
           ,@body
           t)))))

(define-order-method order-up () (< (1+ i) (length widgets))
  (setf (aref widgets i) (aref widgets (1+ i))
        (aref widgets (1+ i)) target-widget))

(define-order-method order-down () (> i 0)
  (setf (aref widgets i) (aref widgets (1- i))
        (aref widgets (1- i)) target-widget))

(define-order-method order-top () (not (= i (1- (length widgets))))
  (setf widgets
        (make-array (length widgets)
           :initial-contents
           (concatenate 'vector
             (subseq widgets 0 i)
             (subseq widgets (1+ i))
             (subseq widgets i (1+ i)))
           :adjustable t)))

(define-order-method order-bottom () (> i 0)
  (setf widgets
        (make-array (length widgets)
           :initial-contents
           (concatenate 'vector
             (subseq widgets i (1+ i))
             (subseq widgets 0 i)
             (subseq widgets (1+ i)))
           :adjustable t)))

(defmethod draw ((container container))
  (with-slots (visible) container
    (unless visible
      (return-from draw))
    (draw-background container)
    (loop for widget across (slot-value container 'widgets)
       do (draw widget))))

(defmethod draw-background ((container container))
  (with-slots (width height background) container
    (when (null background)
      (return-from draw-background))
    (draw-sprite background :width width :height height :mode :tile)))

(declaim (inline set-up-root-container add-root-listener 
                 remove-root-listener add-to-root
                 remove-from-root widget-of-root
                 root-tag-of root-tag-widget
                 root-remove-tag))
(internal set-up-root-container)
(defun set-up-root-container ()
  (setf *root-container* (make-instance 'container :clipping nil)))
(defun add-root-listener (listener &optional event-type)
  (subscribe *root-container* listener event-type))
(defun remove-root-listener (listener &optional event-type)
  (unsubscribe *root-container* listener event-type))
(defun add-to-root (widget &optional tag)
  (add-widget *root-container* widget tag))
(defun remove-from-root (widget &key (unsubscribes t))
  (remove-widget *root-container* widget :unsubscribes unsubscribes))
(defun widget-of-root (tag)
  (widget-of *root-container* tag))
(defun root-tag-of (widget)
  (tag-of *root-container* widget))
(defun root-tag-widget (widget tag)
  (tag-widget *root-container* widget tag))
(defun root-remove-tag (identifier)
  (remove-tag *root-container* identifier))
