; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(define-condition tag-error (error)
  ((fault :initarg :fault
          :initform :fault)
   (tag :initarg :tag
        :initform nil)
   (widget :initarg :widget
           :initform nil))
  (:report (lambda (condition stream)
             (with-slots (fault tag widget) condition
               (case fault
                 (:tag (format stream "Tag ~s for widget ~s must be unique"
                               tag widget))
                 (:widget (format stream "Widget ~s already has tag ~s"
                                  widget tag))
                 (:invalid-tag (format stream "Couldn't find tag ~s" tag))
                 (otherwise (format stream "Unknown fault: ~s" fault)))))))

(defclass screen (widget)
  ((visible :initarg :visible
            :initform t)
   (provided-events :initform '(:mouse-move :mouse-down :mouse-up :parent-move))
   (desired-events :initform '(:title-bar-drag event-title-bar-drag))
   (widgets :initform '()
            :reader widgets)
   (background :initform nil
               :reader background)
   (tags :initform '())))

(defmethod initialize-instance :after ((screen screen) &key
                                       (manager *screen-manager*))
  (unless (null manager)
    (check-type manager screen-manager)
    (sm-add-screen manager screen)))

(defmethod tag-widget ((screen screen) (widget widget) tag)
  (with-slots (tags) screen
    (multiple-value-bind (fault tag widget)
        (loop
           for (tag-key tag-value) on tags by #'cddr do
             (cond
               ((eq tag-key tag)
                (return (values :tag tag-key tag-value)))
               ((eq tag-value widget)
                (return (values :widget tag-key tag-value))))
           finally
             (return (values nil nil nil)))
      (assert (not fault) (tag widget)
              'tag-error :fault fault :tag tag :widget widget))
    (setf (getf tags tag) widget)))

(defmethod remove-tag ((screen screen) identifier)
  (with-slots (tags) screen
    (when (null tags) (return-from remove-tag))
    (setf tags (loop for (key value) on tags by #'cddr
                  unless (eq (if (subtypep (type-of identifier) 'widget) 
                                 value key)
                             identifier)
                  collect key and collect value))))

(defmethod widget-of ((screen screen) tag)
  (let ((value (getf (slot-value screen 'tags) tag)))
    (assert value (value) 'tag-error :fault :invalid-tag :tag tag)
    value))

(defmethod tag-of ((screen screen) (widget widget))
  (loop for (key value) on (slot-value screen 'tags) by #'cddr
       when (eql value widget) do (return key)
       finally (return nil)))

(defmethod add-widget ((screen screen) widget &optional tag)
  (check-type widget widget)
  (setf (parent widget) screen)
  (with-slots (widgets) screen
    (pushnew widget widgets))
  (loop for event-type in (desired-events widget) by #'cddr
     do (add-listener screen widget event-type))
  (when (keywordp tag)
    (tag-widget screen widget tag)))

(defmethod remove-widget ((screen screen) widget &key (remove-listeners t))
  (with-slots (widgets tags) screen
    (setf widgets (delete-if (lambda (other-widget) (eq widget other-widget))
                             widgets))
    (remove-tag screen widget)
    (when remove-listeners
      (dolist (event-type (desired-events widget))
        (remove-listener screen widget event-type)))))

(defmethod draw ((screen screen))
  (with-slots (x y) screen
    (with-translate (x y)
      (draw-panel screen)
      (dolist (widget (slot-value screen 'widgets))
        (draw widget)))))

(defmethod draw-panel ((screen screen))
  (with-slots (x y width height background) screen
    (when (null background)
      (return-from draw-panel))
    (draw-tiled background x y :width width :height height)))
