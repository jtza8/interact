; Copyright 2009 Jens Thiede. All rights reserved.
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
           :reader height)))

; Not working correctly yet in this commit. Change of implementation.
(defmethod within ((widget widget) x y)
  (with-slots ((this-x x) (this-y y) width height) widget
    (and (< this-x x (+ this-x width))
         (< this-y y (+ this-y height)))))