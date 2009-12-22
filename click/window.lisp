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
   (x :initform 100)
   (y :initform 100)
   (width :initform 1000)
   (height :initform 1000)))

(defmethod draw ((window window))
  (assert-window-manager-exists)
  (with-slots (x y width height) window
    (gl:with-primitive :quads
      (gl:vertex (+ x width) (+ y height)))))