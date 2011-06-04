; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass igo (listenable listener)
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
   (width :initform 50
          :initarg :width
          :reader width)
   (height :initform 50
           :initarg :height
           :reader height)
   (parent :initform nil
           :initarg :parent
           :accessor parent)))

(defmethod draw :around ((igo igo))
  (with-slots (x y pivot-x pivot-y rotation width height) igo
    (gl:matrix-mode :modelview)
    (gl:with-pushed-matrix
      (if (not (zerop rotation))
          (progn
            (gl:translate (+ x pivot-x) (+ y pivot-y) 0)
            (gl:rotate rotation 0 0 1)
            (gl:translate (- pivot-x) (- pivot-y) 0))
          (gl:translate x y 0))
      (call-next-method))))

(defmethod absolute-pos ((igo igo))
  (with-slots (parent x y) igo
    (if (typep parent 'igo)
        (destructuring-bind (px py) (absolute-pos parent)
          (list (+ x px) (+ y py)))
        (list x y))))

(defmethod absolute-x ((igo igo))
  (car (absolute-pos igo)))

(defmethod absolute-y ((igo igo))
  (cadr (absolute-pos igo)))

(defmethod within ((igo igo) x y)
  (with-slots (width height) igo
    (destructuring-bind (ax ay) (absolute-pos igo)
      (and (< ax x (+ ax width))
           (< ay y (+ ay height))))))

(defmethod forward-standard-events ((igo igo))
  (provide-events igo :display-state :key-down :key-up :mouse-motion
                  :mouse-button-down :mouse-button-up :parent-move 
                  :joy-axis-motion :joy-hat-motion :joy-ball-motion
                  :joy-button-down :joy-button-up :quit :sys-wm-event
                  :display-resize :display-exposure :user-event
                  :before-frame :after-frame :display-update)
  (desire-events igo :display-state #'send-event :key-down #'send-event 
                 :key-up #'send-event :mouse-motion #'send-event
                 :mouse-button-down #'send-event :mouse-button-up #'send-event 
                 :parent-move #'send-event :joy-axis-motion #'send-event
                 :joy-hat-motion #'send-event :joy-ball-motion #'send-event
                 :joy-button-down #'send-event :joy-button-up #'send-event
                 :quit #'send-event :sys-wm-event #'send-event 
                 :display-resize #'send-event :display-exposure #'send-event
                 :user-event #'send-event :before-frame #'send-event
                 :after-frame #'send-event :display-update #'send-event))