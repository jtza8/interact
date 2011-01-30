; Copyright 2011 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass widget (listenable listener)
  ((x :initform 0
      :initarg :x
      :reader x)
   (y :initform 0
      :initarg :y
      :reader y)
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
   (height :initform 20
           :initarg :height
           :reader height)
   (parent :initform nil
           :initarg :parent
           :accessor parent)))

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