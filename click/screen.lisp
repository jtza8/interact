; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

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

(defclass screen (igo)
  ((visible :initarg :visible
            :initform t)
   (igos :initform '()
            :reader igos)
   (background :initform nil
               :reader background)
   (tags :initform '())))

(defmethod initialize-instance :after ((screen screen) &key
                                       (manager *screen-manager*))
  (unless (null manager)
    (check-type manager screen-manager)
    (sm-add-screen manager screen))
  (provide-events screen :mouse-move :mouse-down :mouse-up :parent-move))

(defmethod tag-igo ((screen screen) (igo igo) tag)
  (with-slots (tags) screen
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

(defmethod remove-tag ((screen screen) identifier)
  (with-slots (tags) screen
    (when (null tags) (return-from remove-tag))
    (setf tags (loop for (key value) on tags by #'cddr
                  unless (eq (if (subtypep (type-of identifier) 'igo) 
                                 value key)
                             identifier)
                  collect key and collect value))))

(defmethod igo-of ((screen screen) tag)
  (let ((value (getf (slot-value screen 'tags) tag)))
    (assert value (value) 'tag-error :fault :invalid-tag :tag tag)
    value))

(defmethod tag-of ((screen screen) (igo igo))
  (loop for (key value) on (slot-value screen 'tags) by #'cddr
       when (eql value igo) do (return key)
       finally (return nil)))

(defmethod add-igo ((screen screen) igo &optional tag)
  (check-type igo igo)
  (setf (parent igo) screen)
  (with-slots (igos) screen
    (pushnew igo igos))
  (loop for event-type in (desired-events igo) by #'cddr
     do (add-listener screen igo event-type))
  (when (keywordp tag)
    (tag-igo screen igo tag)))

(defmethod remove-igo ((screen screen) igo &key (remove-listeners t))
  (with-slots (igos tags) screen
    (setf igos (delete-if (lambda (other-igo) (eq igo other-igo))
                             igos))
    (remove-tag screen igo)
    (when remove-listeners
      (dolist (event-type (desired-events igo))
        (remove-listener screen igo event-type)))))

(defmethod draw ((screen screen))
  (with-slots (x y) screen
    (with-translate (x y)
      (draw-panel screen)
      (dolist (igo (slot-value screen 'igos))
        (draw igo)))))

(defmethod draw-panel ((screen screen))
  (with-slots (x y width height background) screen
    (when (null background)
      (return-from draw-panel))
    (draw-at background x y :width width :height height :mode :tile)))
