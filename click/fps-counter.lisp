; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass fps-counter (igo)
  ((timer :initform (make-instance 'stopwatch))
   (font-sprite :initform (error "must specify font-sprite")
                :initarg :font-sprite)))

(defmethod initialize-instance :after ((igo fps-counter) &key)
  (desire-events igo :before-frame #'before-frame-event))

(defmethod before-frame-event ((igo fps-counter) event)
  (declare (ignore event))
  (with-slots (timer font-sprite) igo
    (if (= (lap timer) 0)
        (start timer)
        (setf (text font-sprite)
              (format nil "FPS: ~,2f" (/ 1000 (lap timer)))))
    (reset timer :auto-start t)))

(defmethod draw ((igo fps-counter))
  (with-slots (font-sprite) igo
    (draw-sprite font-sprite)))