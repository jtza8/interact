; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass animation-sprite (sprite)
  ((fps :initarg :fps
        :initform (error "must specify fps")
        :accessor fps)
   (sprite-vector :initarg :sprite-vector
                  :initform (error "must specify sprite vector"))
   (watch :initform (make-instance 'watch))
   (repeating :initform t
              :initarg :looping)))

(define-instance-maker animation-sprite)

(defmethod initialize-instance :after ((sprite animation-sprite) &key (start t))
  (with-slots (sprite-vector watch) sprite
    (check-type sprite-vector vector)
    (assert (> (length sprite-vector) 0))
    (when start (start watch))))

(defmethod diverge ((sprite animation-sprite) &rest init-args)
  (with-slots (height width fps sprite-vector repeating) sprite
    (apply #'make-instance 'animation-sprite
           (append init-args
                   (list :height height :width width
                         :fps fps :sprite-vector sprite-vector
                         :looping repeating)))))

(defmethod draw-sprite ((sprite animation-sprite) &key (x 0) (y 0) width height)
  (with-slots (fps sprite-vector watch repeating) sprite
    (let* ((frame-counter (truncate (* (lap watch :sec) fps)))
           (frame-count (length sprite-vector))
           (frame-number (if repeating
                             (rem frame-counter frame-count)
                             (if (>= frame-counter frame-count)
                                 (progn
                                   (when (running-p watch)
                                     (stop watch))
                                   (1- frame-count))
                                 frame-counter))))
      (draw-sprite (aref sprite-vector frame-number) :x x :y y
                   :width width :height height))))

(defmethod free ((sprite animation-sprite))
  (with-slots (sprite-vector) sprite
    (map nil #'free sprite-vector)))

(macrolet ((messages-to-watch (&rest messages)
             `(progn
                ,@(loop for message in messages
                        collect `(defmethod ,message ((sprite animation-sprite))
                                   (with-slots (watch) sprite
                                     (,message watch)))))))
  (messages-to-watch start stop))
