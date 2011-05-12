; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass bitmap-font-sprite (sprite)
  ((glyph-width :initform (error "must specify glyph-width")
                :initarg :glyph-width
                :reader glyph-width)
   (glyph-height :initform (error "must specify glyph-height")
                 :initarg :glyph-height
                 :reader glyph-height)
   (glyph-vector :initform (error "must specify glyph-vector")
                 :initarg :glyph-vector)
   (ascii-offset :initform (error "must specify ascii-offset")
                 :initarg :ascii-offset)
   (tracking :initform (error "must specify tracking")
             :initarg :tracking
             :accessor tracking)
   (width :initform 0)
   (height :initform 0)
   (message :initform ""
            :reader message)))

(defmethod (setf message) (value (sprite bitmap-font-sprite))
  (with-slots (width height glyph-width glyph-height
               glyph-vector message) sprite
    (setf message value
          width (* glyph-width (length message))
          height (if (> (length message) 0) glyph-height 0))))

(defmethod initialize-instance :after ((sprite bitmap-font-sprite) &key)
  ())

(defmethod fetch-glyph ((sprite bitmap-font-sprite) char)
  (with-slots (glyph-vector ascii-offset) sprite
    (let ((glyph-index (- (char-code char) ascii-offset)))
      (assert (and (<= 0 glyph-index)
                   (< glyph-index (length glyph-vector)))
              () "index error")
      (aref glyph-vector glyph-index))))

(defmethod draw-sprite ((sprite bitmap-font-sprite) &key
                        (x 0) (y 0) width height mode)
  (with-slots (message glyph-width tracking) sprite
    (loop for char across message
          for i upfrom 0
          when (not (char= char #\Space))
          do (draw-sprite (fetch-glyph sprite char)
                          :x (+ x (* i (+ glyph-width tracking)))
                          :y y))))