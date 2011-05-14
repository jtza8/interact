; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass animation-sprite (sprite)
  ((fps :initarg :fps
        :initform (error "must specify fps")
        :reader fps)
   (sprite-vector :initarg :sprite-vector
                  :initform (error "must specify sprite vector"))
   (stopwatch :initform (make-instance 'stopwatch))))

(defmethod initialize-instance :after ((sprite animation-sprite) &key (start t))
  (with-slots (sprite-vector stopwatch) sprite
    (check-type sprite-vector vector)
    (assert (> (length sprite-vector) 0))
    (when start (start stopwatch))))

(defmethod diverge ((sprite animation-sprite))
  (with-slots (height width fps sprite-vector) sprite
    (make-instance 'animation-sprite
                   :height height :width width
                   :fps fps :sprite-vector sprite-vector)))

(defmethod draw-sprite ((sprite animation-sprite) &key (x 0) (y 0))
  (with-slots (fps sprite-vector stopwatch) sprite
    (let ((frame-number (rem (truncate (/ (* (lap stopwatch) fps) 1000))
                             (length sprite-vector))))
      (draw-sprite (aref sprite-vector frame-number) :x x :y y))))

(defmethod free ((sprite animation-sprite))
  (with-slots (sprite-vector) sprite
    (map nil #'free sprite-vector)))

(macrolet ((messages-to-stopwatch (&rest messages)
             `(progn
                ,@(loop for message in messages
                        collect `(defmethod ,message ((sprite animation-sprite))
                                   (with-slots (stopwatch) sprite
                                     (,message stopwatch)))))))
  (messages-to-stopwatch start stop))
