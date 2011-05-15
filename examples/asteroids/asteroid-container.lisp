; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click-examples)

(defclass asteroid-container (container)
  ())

(define-instance-maker asteroid-container)

(defmethod initialize-instance :after ((container asteroid-container) &key)
  (desire-events container
                 :asteroid-explosion #'asteroid-explosion-event)
                 ;; :before-frame #'before-frame-event)
  (provide-events container :asteroid-collision))

(defmethod asteroid-explosion-event ((container asteroid-container) event)
  (with-event-keys (source) event
    (remove-igo container source)
    (let ((x (x source))
          (y (y source))
          (new-size (case (size source)
                      (:large :medium)
                      (:medium :small))))
      (when (eq (size source) :small)
        (return-from asteroid-explosion-event))
      (add-igo container (make-instance 'asteroid :size new-size :x x :y y))
      (add-igo container (make-instance 'asteroid :size new-size :x x :y y))
      (add-igo container (make-instance 'asteroid :size new-size :x x :y y)))))

(defmethod before-frame-event ((container asteroid-container) event)
  (let ((listeners (getf (slot-value container 'listeners)
                         :asteroid-collision)))
    (dolist (a listeners)
      (dolist (b listeners)
        (when (eq a b)
          (go continue))
        (when (or (within a (x b) (y b))
                  (within a (+ (width b) (x b)) (+ (height b) (y b))))
          (unless (and (find a listeners) (find b listeners))
            (go continue))
          (let ((handler-a (select-handler a :asteroid-collision))
                (handler-b (select-handler b :asteroid-collision)))
            (unless (or (null handler-a) (null handler-b))
              (funcall handler-a a (list :asteroid-collision :collider b))
              (funcall handler-b b (list :asteroid-collision :collider a))))))
        continue)))