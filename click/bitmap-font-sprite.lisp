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
   (message :initform "doo"
            :accessor message)))

(defmethod initialize-instance :after ((sprite bitmap-font-sprite) &key)
  ())

(defmethod fetch-glyph ((sprite bitmap-font-sprite) char)
  (with-slots (glyph-vector ascii-offset) sprite
    (assert (char< #\A char #\~) () "index error")
    (aref glyph-vector (- (char-code char) ascii-offset))))

(defmethod draw-sprite ((sprite bitmap-font-sprite) &key (x 0) (y 0) width height mode)
  (with-slots (message glyph-width) sprite
    (loop for char across message
          for i upfrom 0
          do (draw-sprite (fetch-glyph sprite char)
                          :x (+ x (* i glyph-width))
                          :y y))))