; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass polygon-sprite (vector-sprite)
  ((points :initform #()
           :initarg :points
           :reader points)
   (fill-colour :initform '(0.5 0.5 0.5 1.0)
                :initarg :fill-colour
                :reader fill-colour)
   (line-colour :initform '(0.2 0.2 0.2 1.0)
               :initarg :line-colour
               :reader line-colour)
   (line-width :initform 1
               :initarg :line-width
               :reader :line-colour)))

(define-instance-maker polygon-sprite)

(defmethod initialize-instance :after ((sprite polygon-sprite) &key)
  (update-texture sprite))

(defmethod update-texture ((sprite polygon-sprite))
  (with-slots (texture width height points fill-colour 
               line-colour line-width) sprite
    (setf texture
          (with-vecto-canvas-as-texture (width height)
            (vecto:set-line-width line-width)
            (apply #'vecto:set-rgba-stroke line-colour)
            (apply #'vecto:set-rgba-fill fill-colour)
            (loop for i from 1 upto (1- (length points))
                  for point = (aref points i)
                  initially (vecto:move-to (aref (aref points 0) 0)
                                           (aref (aref points 0) 1))
                  do (vecto:line-to (aref point 0) (aref point 1))
                  finally 
                    (progn
                      (vecto:close-subpath)
                      (vecto:fill-and-stroke)))))))

(define-vector-sprite-writers polygon-sprite
  points fill-colour line-colour line-width)