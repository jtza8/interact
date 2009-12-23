; Copyright 2009 Jens Thiede. All rights reserved.
; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass window (widget)
  ((title :initarg :title
          :initform "Untitled")
   (visible :initarg :visible
            :initform nil)
   (tags :initform '())
   (x :initform 10)
   (y :initform 10)
   (width :initform 100)
   (height :initform 100)))

(defmethod initialize-instance :after ((window window) &key)
  (assert-window-manager-exists)
  (add-window *window-manager* window))

(defmethod draw ((window window))
  (with-slots (x y width height) window
    (gl:color 1 1 1)
    (gl:with-primitive :quads
      (gl:vertex x y)
      (gl:vertex (+ x width) y)
      (gl:vertex (+ x width) (+ y height))
      (gl:vertex x (+ y height)))))