; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :interact-examples)

(defclass asteroid (widget)
  ((size :initform :large
         :initarg :size
         :reader size)
   (rotation-speed :initform (- (random 101) 50))
   (velocity :initform (cons (random 2000) (random 360)))
   sprite
   offset))

(define-instance-maker asteroid)

(defmethod initialize-instance :after ((asteroid asteroid) &key)
  (with-slots (size sprite offset rotation velocity
               width height pivot-x pivot-y)
      asteroid
    (setf sprite (clone (sprite-node :asteroid :medium))
          width (width sprite)
          height (height sprite)
          offset (- (sqrt (+ (expt width 2) (expt height 2))))
          (car velocity) (if (= (car velocity) 0) 
                             (1+ (random 5))
                             (car velocity))
          rotation (random 360)
          pivot-x (/ width 2)
          pivot-y (/ height 2))
    (desire-events asteroid :after-frame #'after-frame-event
                   :asteroid-collision #'asteroid-collision-event)
    (provide-events asteroid :asteroid-explosion)))

(defmethod draw ((asteroid asteroid))
  (with-slots (sprite) asteroid
    (draw-sprite sprite)))

(defmethod asteroid-collision-event ((asteroid asteroid) event)
  (with-slots (size parent) asteroid
    (with-event-keys (collider) event
      (when (or (and (eq (size collider) :large) (eq size :medium))
                (and (eq (size collider) :medium) (eq size :small)))
        (let ((handler (select-handler parent :asteroid-explosion)))
          (unless (null handler)
            (funcall handler parent
                     (list :asteroid-explosion :source asteroid))))))))

(defmethod after-frame-event ((asteroid asteroid) event)
  (with-slots (offset rotation velocity rotation-speed height width x y)
      asteroid
    (let ((time-delta (lap interact:*iter-watch* :sec)))
      (incf rotation (* rotation-speed time-delta))
      (incf x (* (car velocity) time-delta (cos (cdr velocity))))
      (incf y (* (car velocity) time-delta (sin (cdr velocity))))
      (cond ((< x offset) (setf x (screen-width)))
            ((> x (screen-width)) (setf x offset)))
      (cond ((< y offset) (setf y (screen-height)))
            ((> y (screen-height)) (setf y offset))))))