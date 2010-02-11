; Copyright 2009 Jens Thiede. All rights reserved.
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

(defclass window (widget)
  ((visible :initarg :visible
            :initform t)
   (listenable-events :initform '(:mouse-move :mouse-down :mouse-up))
   (widgets :initform '()
            :reader widgets)
   (tags :initform '())))

(defmethod initialize-instance :after ((window window) &key)
  (assert-window-manager-exists)
  (with-slots (left-margin right-margin top-margin bottom-margin
               width x y) window
    (with-node-sprites (:window :shadows) (corner-left-top corner-right-bottom)
      (setf left-margin (width corner-left-top)
            right-margin (width corner-right-bottom)
            top-margin (height corner-left-top)
            bottom-margin (height corner-right-bottom)))
    (let ((title-bar (make-instance 'title-bar 
                                    :width width
                                    :x-offset x
                                    :y-offset y)))
      (add-widget window title-bar)
      (tag-widget window title-bar :title-bar)
      (add-listener title-bar window :dragging))
    (add-window *window-manager* window)))

(defmethod tag-widget ((window window) (widget widget) tag)
  (with-slots (tags) window
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

(defmethod widget ((window window) tag)
  (let ((value (getf (slot-value window 'tags) tag)))
    (assert value (value) 'tag-error :fault :invalid-tag :tag tag)
    value))

(defmethod add-widget ((window window) new-widget)
  (check-type new-widget widget)
  (with-slots (widgets) window
    (pushnew new-widget widgets))
  (dolist (event-type (listen-for-events new-widget))
    (add-listener window new-widget event-type)))

(defmethod remove-widget ((window window) target-widget)
  (with-slots (widgets) window
    (setf widgets (delete-if (lambda (widget) (eq target-widget widget))
                             widgets))))

(defmethod draw ((window window))
  (draw-shadows window)
  (draw-panel window)
  (dolist (widget (slot-value window 'widgets))
    (draw widget)))

(defmethod draw-shadows ((window window))
  (with-slots (width height left-margin right-margin
               top-margin bottom-margin) window
    (let ((ax (abs-x window))
          (ay (abs-y window)))
      (with-node-sprites (:window :shadows)
          (corner-left-top top-left top-centre top-right
           corner-right-top right-top right-centre right-bottom
           corner-right-bottom bottom-right bottom-centre bottom-left
           corner-left-bottom left-bottom left-centre left-top)
        (draw-at corner-left-top (- ax left-margin) (- ay top-margin))
        (draw-at top-left ax (- ay top-margin))
        (draw-tiled top-centre (+ ax (width top-left)) (- ay top-margin)
                    :width (- width (width top-left) (width top-right)))
        (draw-at top-right (+ ax (- width (width top-right))) (- ay top-margin))
        (draw-at corner-right-top (+ ax width) (- ay top-margin))
        (draw-at right-top (+ ax width) ay)
        (draw-tiled right-centre (+ ax width) (+ ay (height right-top))
                    :height (- height 
                               (height right-top)
                               (height right-bottom)))
        (draw-at right-bottom
                 (+ ax width) (+ ay (- height (height right-bottom))))
        (draw-at corner-right-bottom (+ ax width) (+ ay height))
        (draw-at bottom-right
                 (+ ax (- width (width bottom-right))) (+ ay height))
        (draw-tiled bottom-centre (+ ax (width bottom-left)) (+ ay height)
                    :width (- width (width bottom-left) (width bottom-right)))
        (draw-at bottom-left ax (+ ay height))
        (draw-at corner-left-bottom (- ax left-margin) (+ ay height))
        (draw-at left-bottom
                 (- ax left-margin)
                 (+ ay (- height (height left-bottom))))
        (draw-tiled left-centre (- ax left-margin) (+ ay (height left-top))
                    :height (- height (height left-bottom) (height left-top)))
        (draw-at left-top (- ax left-margin) ay)))))

(defmethod draw-panel ((window window))
  (with-slots (width height title-bar) window
    (let ((ax (abs-x window))
          (ay (abs-y window))
          (title-bar-height (height (widget window :title-bar))))
      (with-node-sprites (:window :panel)
          (left corner-left-bottom bottom corner-right-bottom right background)
        (draw-tiled left ax (+ ay title-bar-height)
                    :height (- height title-bar-height
                               (height corner-left-bottom)))
        (draw-at corner-left-bottom
                 ax (+ ay (- height (height corner-left-bottom))))
        (draw-tiled bottom
                    (+ ax (width corner-left-bottom))
                    (+ ay (- height (height bottom)))
                    :width (- width
                              (width corner-left-bottom)
                              (width corner-right-bottom)))
        (draw-at corner-right-bottom
                 (+ ax (- width (width corner-right-bottom)))
                 (+ ay (- height (height corner-right-bottom))))
        (draw-tiled right (+ ax (- width (width right))) (+ ay title-bar-height)
                    :height (- height title-bar-height
                               (height corner-right-bottom)))
        (draw-tiled background
                    (+ ax (width left)) (+ ay title-bar-height)
                    :height (- height title-bar-height (height bottom))
                    :width (- width (width left) (width right)))))))

(defmethod select-handler ((window window) event-type)
  (when (eq event-type :dragging)
    #'event-dragging))

(defmethod event-dragging ((window window) event)
  (with-slots (x y) window
    (with-event-keys (x-offset y-offset) event
      (setf x x-offset
            y y-offset))))