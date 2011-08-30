; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact)

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
   (colour :initform '(1.0 1.0 1.0 1.0)
          :initarg :colour
          :accessor colour)
   (width :initform 0)
   (height :initform 0)
   (text :initform ""
         :reader text)))

(define-instance-maker bitmap-font-sprite)

(defmethod (setf text) (value (sprite bitmap-font-sprite))
  (with-slots (width height glyph-width glyph-height
               glyph-vector text) sprite
    (setf text value
          width (* glyph-width (length text))
          height (if (> (length text) 0) glyph-height 0))))

(internal fetch-glyph)
(defmethod fetch-glyph ((sprite bitmap-font-sprite) char)
  (with-slots (glyph-vector ascii-offset) sprite
    (let ((glyph-index (- (char-code char) ascii-offset)))
      (assert (and (<= 0 glyph-index)
                   (< glyph-index (length glyph-vector)))
              (char) 'bitmap-char-error :character char)
      (aref glyph-vector glyph-index))))

(defmethod draw-sprite ((sprite bitmap-font-sprite) &key (x 0) (y 0))
  (with-slots (text glyph-width tracking colour) sprite
    (gl:with-pushed-attrib (:current-bit)
      (apply #'gl:color colour)
      (loop for char across text
         for i upfrom 0
         when (not (char= char #\Space))
          do (draw-sprite (fetch-glyph sprite char)
                          :x (+ x (* i (+ glyph-width tracking)))
                          :y y)))))

(defmethod clone ((sprite bitmap-font-sprite) &rest init-args)
  (with-slots (ascii-offset glyph-width glyph-height 
               glyph-vector tracking colour) sprite
    (apply #'make-instance 'bitmap-font-sprite
           (append init-args
                   (list :ascii-offset ascii-offset
                         :glyph-width glyph-width
                         :glyph-height glyph-height
                         :glyph-vector glyph-vector
                         :tracking tracking
                         :colour colour)))))

(defmethod free ((sprite bitmap-font-sprite))
  (with-slots (glyph-vector) sprite
    (loop for glyph across glyph-vector
          do (free glyph))))
