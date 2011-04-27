; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click-examples)

(defclass asteroid (igo)
  ((size :initform :large
         :initarg :size
         :reader size)
   (rotation-speed :initform (- (random 11) 5))
   (velocity :initform (cons (random 5) (random 360)))
   sprite
   offset))

(defmethod initialize-instance :after ((asteroid asteroid) &key)
  (with-slots (size sprite offset rotation width height pivot-x pivot-y)
      asteroid
    (setf sprite (diverge (sprite-node :asteroid size))
          width (width sprite)
          height (height sprite)
          offset (- (sqrt (+ (expt width 2) (expt height 2))))
          rotation (random 360)
          pivot-x (/ width 2)
          pivot-y (/ height 2))))

(defmethod draw ((asteroid asteroid))
  (with-slots (sprite offset rotation velocity rotation-speed height width x y)
      asteroid
    (draw-sprite sprite)
    (incf rotation rotation-speed)
    (incf x (* (car velocity) (cos (cdr velocity))))
    (incf y (* (car velocity) (sin (cdr velocity))))
    (cond ((< x offset) (setf x 800))
          ((> x 800) (setf x offset)))
    (cond ((< y offset) (setf y 600))
          ((> y 600) (setf y offset)))))