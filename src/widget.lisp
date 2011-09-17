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
   (width :initform 0
          :initarg :width
          :reader width)
   (height :initform 0
           :initarg :height
           :reader height)
   (parent :initform nil
           :initarg :parent
           :accessor parent)))

(defmethod init-sprites ((widget widget)))

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
  (with-slots (parent x) widget
    (if (typep parent 'widget)
        (+ x (absolute-x parent))
        x)))

(defmethod absolute-y ((widget widget))
  (with-slots (parent y) widget
    (if (typep parent 'widget)
        (+ y (absolute-y parent))
        y)))

(defmethod within ((widget widget) x y)
  (with-slots (width height) widget
    (destructuring-bind (ax ay) (absolute-pos widget)
      (and (< ax x (+ ax width))
           (< ay y (+ ay height))))))

(defmethod forward-standard-events ((widget widget))
  (provide-events widget :key-down :key-up :char-down :char-up :mouse-pos
                  :mouse-down :mouse-up :mouse-wheel :parent-move :quit
                  :window-size :window-refresh :before-frame :after-frame
                  :display-update)
  (desire-events widget :key-down #'send-event :key-up #'send-event
                 :char-down #'send-event :char-up #'send-event
                 :mouse-pos #'send-event :mouse-down #'send-event
                 :mouse-up #'send-event :mouse-wheel #'send-event
                 :parent-move #'send-event :quit #'send-event
                 :window-size #'send-event :window-refresh #'send-event
                 :before-frame #'send-event :after-frame #'send-event
                 :display-update #'send-event))
