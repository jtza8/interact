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
                 (otherwise (format stream "Unknown fault: ~s" fault))))))
  (:documentation 
   "Signals a tag error and generates reports as specified by the
`:fault` key as follows:

`:fault` Value | Meaning
-------------- | -------------------------------------
`:tag`         | The tag is not unique to the window.  
`:widget`      | A widget already has another tag.  
`:invalid-tag` | The tag doesn't exist in the window."))

(defclass window (widget)
  ((visible :initarg :visible
            :initform t)
   (provided-events :initform '(:mouse-move :mouse-down :mouse-up))
   (desired-events :initform '(:title-bar-drag event-title-bar-drag))
   (widgets :initform '()
            :reader widgets)
   (tags :initform '()))
  (:documentation "Windows are widgets which contain other widgets."))

(defmethod initialize-instance :after ((window window) &key)
  "Initialises an instance of a `window` object as follows:

1. Assert that a default window manager exists.
2. Calculates the margins of the window, namely the whitespace around
the window. Margins allow shadows to be drawn outside of the window
without disturbing the actual dimensions of the window.
3. Creates an instance of the `task-bar` widget which it adds to its
collection of widgets and then tags as `:title-bar`.
4. Finally, the window is added to the default window manager, as
specified by the global variable, `*window-manager*`."
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
      (add-listener title-bar window :title-bar-drag))
    ; Should an alternative window manager be specifiable via a key?
    (add-window *window-manager* window)))

(defmethod tag-widget ((window window) (widget widget) tag)
  "Tags a widget with the specified tag. A tag alows for the easy
recall of a widget without having to name every widget present in the
window. Tags can only be specified in a window (This is due the the
design philosophy that, \"Flat is better than nested\"). Widgets may
only be tagged once within a window."
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

(defmethod remove-tag ((window window) identifier)
  "Removes a tag from a window. Identifier may be either a symbol or a
widget."
  (with-slots (tags) window
    (when (null tags) (return-from remove-tag))
    (setf tags (loop for (key value) on tags by #'cddr
                  unless (eq (if (subtypep (type-of identifier) 'widget) 
                                 value key)
                             identifier)
                  collect key and collect value))))

(defmethod widget-of ((window window) tag)
  "Returns the widget specified by `tag`. If widget isn't found, then
a `tag-error` condition is signaled. It is expected that a tag always
points to a widget, but it's not expected that a widget always has a
tag."
  (let ((value (getf (slot-value window 'tags) tag)))
    (assert value (value) 'tag-error :fault :invalid-tag :tag tag)
    value))

(defmethod tag-of ((window window) (widget widget))
  "Returns the tag associated with `widget`, or `null` if the window
didn't have a tag pointing to `widget`."
  (loop for (key value) on (slot-value window 'tags) by #'cddr
       when (eql value widget) do (return key)
       finally (return nil)))

(defmethod add-widget ((window window) widget)
  "Adds a widget to the window. Adds event listeners as requested by
the widget. Event listeners are specified as a list of `:symbols` in
the instance variable `desired-events`. The content of this
variable are accessable via a reader of the same name. For more
information, see the documentation for the `listener` class."
  (check-type widget widget)
  (with-slots (widgets) window
    (pushnew widget widgets))
  (loop for event-type in (desired-events widget) by #'cddr
        do (add-listener window widget event-type)))

(defmethod remove-widget ((window window) widget &key (remove-listeners t))
  "Removes `widget` from `window`. Automatically removes tags and,
unless told not to, removes event listeners as specified by the
`desired-events` reader belonging to `widget`."
  (with-slots (widgets tags) window
    (setf widgets (delete-if (lambda (other-widget) (eq widget other-widget))
                             widgets))
    (remove-tag window widget)
    (when remove-listeners
      (dolist (event-type (desired-events widget))
        (remove-listener window widget event-type)))))

(defmethod draw ((window window))
  "Draws the shadows, then the panel, then the widgets."
  (draw-shadows window)
  (draw-panel window)
  (dolist (widget (slot-value window 'widgets))
    (draw widget)))

(defmethod draw-shadows ((window window))
  "Draws the window's shadows."
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
  "Draws the panel, that is, the background on which widgets will be drawn."
  (with-slots (width height title-bar) window
    (let ((ax (abs-x window))
          (ay (abs-y window))
          (title-bar-height (height (widget-of window :title-bar))))
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

;; (defmethod select-handler ((window window) event-type)
;;   (when (eq event-type :title-bar-drag)
;;     #'event-title-bar-drag))

(defmethod event-title-bar-drag ((window window) event)
  "Adjusts `window`'s x and y coordinates relative to the `:title-bar` widget."
  (with-slots (x y) window
    (with-event-keys (x-offset y-offset) event
      (setf x x-offset
            y y-offset))))