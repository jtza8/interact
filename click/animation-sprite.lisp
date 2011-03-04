; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass animation-sprite (sprite)
  ((fps :initarg :fps
        :initform (error 'program-error "must specify fps")
        :reader fps)
   (sprite-vector :initarg :sprite-vector
                  :initform (error 'program-error "must specify sprite vector"))
   (stopwatch :initform (make-instance 'stopwatch))))

(defmethod initialize-instance :after ((sprite animation-sprite) 
                                       &key (start t))
  (with-slots (sprite-vector stopwatch) sprite
    (check-type sprite-vector vector)
    (assert (> (length sprite-vector) 0))
    (when start (start stopwatch))))

(defmethod draw-at ((sprite animation-sprite) x y)
  (with-slots (fps sprite-vector stopwatch) sprite
    (let ((frame-number (rem (truncate (/ (* (lap stopwatch) fps) 1000))
                             (length sprite-vector))))
      (draw-at (aref sprite-vector frame-number) x y))))

(macrolet ((messages-to-stopwatch (&rest messages)
             `(progn
                ,@(loop for message in messages
                        collect `(defmethod ,message ((sprite animation-sprite))
                                   (with-slots (stopwatch) sprite
                                     (,message stopwatch)))))))
  (messages-to-stopwatch start stop reset))
