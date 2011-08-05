; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

(defclass widget (listenable listener)
  ((x :initform 0
      :initarg :x
      :accessor x)
   (y :initform 0
      :initarg :y
      :accessor y)
   (pivot-x :initform 0
            :initarg :pivot-x
            :accessor pivot-x)
   (pivot-y :initform 0
            :initarg :pivot-y
            :accessor pivot-y)
   (rotation :initform 0
             :initarg :rotation
             :accessor rotation)
   (left-margin :initform 0
                :initarg :left-margin)
   (right-margin :initform 0
                 :initarg :right-margin)
   (top-margin :initform 0
               :initarg :top-margin)
   (bottom-margin :initform 0
                  :initarg :bottom-margin)
   (width :initform 0
          :initarg :width
          :reader width)
   (height :initform 0
           :initarg :height
           :reader height)
   (parent :initform nil
           :initarg :parent
           :accessor parent)))

(defmethod init-sprites ((widget widget)) ())

(defmethod initialize-instance :after ((widget widget) &key)
  (init-sprites widget))

(defmethod draw :around ((widget widget))
  (with-slots (x y pivot-x pivot-y rotation width height) widget
    (gl:matrix-mode :modelview)
    (gl:with-pushed-matrix
      (if (not (zerop rotation))
          (progn
            (gl:translate (+ x pivot-x) (+ y pivot-y) 0)
            (gl:rotate rotation 0 0 1)
            (gl:translate (- pivot-x) (- pivot-y) 0))
          (gl:translate x y 0))
      (call-next-method))))

(defmethod absolute-pos ((widget widget))
  (with-slots (parent x y) widget
    (if (typep parent 'widget)
        (destructuring-bind (px py) (absolute-pos parent)
          (list (+ x px) (+ y py)))
        (list x y))))

(defmethod absolute-x ((widget widget))
  (car (absolute-pos widget)))

(defmethod absolute-y ((widget widget))
  (cadr (absolute-pos widget)))

(defmethod within ((widget widget) x y)
  (with-slots (width height) widget
    (destructuring-bind (ax ay) (absolute-pos widget)
      (and (< ax x (+ ax width))
           (< ay y (+ ay height))))))

(defmethod forward-standard-events ((widget widget))
  (provide-events widget :display-state :key-down :key-up :mouse-motion
                  :mouse-button-down :mouse-button-up :parent-move 
                  :joy-axis-motion :joy-hat-motion :joy-ball-motion
                  :joy-button-down :joy-button-up :quit :sys-wm-event
                  :display-resize :display-exposure :user-event
                  :before-frame :loop-iteration :after-frame :display-update)
  (desire-events widget :display-state #'send-event :key-down #'send-event 
                 :key-up #'send-event :mouse-motion #'send-event
                 :mouse-button-down #'send-event :mouse-button-up #'send-event 
                 :parent-move #'send-event :joy-axis-motion #'send-event
                 :joy-hat-motion #'send-event :joy-ball-motion #'send-event
                 :joy-button-down #'send-event :joy-button-up #'send-event
                 :quit #'send-event :sys-wm-event #'send-event 
                 :display-resize #'send-event :display-exposure #'send-event
                 :user-event #'send-event :before-frame #'send-event
                 :after-frame #'send-event :display-update #'send-event
                 :loop-iteration #'send-event))